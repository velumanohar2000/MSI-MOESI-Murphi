
-- two-state 4-hop VI protocol

----------------------------------------------------------------------
-- Constants,
----------------------------------------------------------------------
const
  ProcCount: 3;          -- number processors
  ValueCount:   2;       -- number of data values.
  VC0: 0;                -- Data, InvAck, InvAckAll, GetMAck, GetSAck, EAck - Low Priority
  VC1: 1;                -- GetS, GetM, PutS, PutM, - Medium Priority
  VC2: 2;                -- FwdGetS, FwdGetM, Inv, PutAck - High Priority
  QMax: 2;
  NumVCs: VC2 - VC0 + 1;
  NetMax: ProcCount+1;
  

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------
type
  Proc: scalarset(ProcCount);   -- unordered range of processors
  Value: scalarset(ValueCount); -- arbitrary values for tracking coherence
  Home: enum { HomeType };      -- need enumeration for IsMember calls
  Node: union { Home , Proc };

  h_state: enum {H_I, H_S, H_M, H_E, H_O, -- stable states
                 H_MSD, H_MMD, H_SMA, H_IE, H_MOD, H_OMA, H_OOD-- transient states
                };
  p_state: enum { P_I, P_S, P_M, P_E, P_O, -- stable states
                  P_ISD, P_IMAD, P_IMA, P_SMAD, P_SMA, P_MIA, P_EIA, P_OMAC, P_OMA, P_OIA, P_SIA, P_IIA --transient states
  };

  VCType: VC0..NumVCs-1;

  MessageType: enum {GetM,
                     GetMAck,
                     GetS,
                     GetSAck,
                     FwdGetM,
                     FwdGetS,
                     Inv,
                     InvAck,
                     PutM,
                     PutE,
                     PutO,
                     PutS,
                     PutAck,
                     InvAckAll,
                     EAck
                    --  MAck
                    };

  Message:
    Record
      mtype: MessageType;
      src: Node;
      -- do not need a destination for verification; the destination is indicated by which array entry in the Net the message is placed
      vc: VCType;
      val: Value;
    End;

  HomeState:
    Record
      state: h_state; -- transient states during recall
      owner: Node;	
      sharers: multiset [ProcCount] of Node;
      val: Value; 
    End;

  ProcState:
    Record
      state: p_state;
      val: Value;
    End;

----------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------
var
  HomeNode:  HomeState;
  Procs: array [Proc] of ProcState;
  Net:   array [Node] of multiset [NetMax] of Message;  -- One multiset for each destination - messages are arbitrarily reordered by the multiset
  InBox: array [Node] of array [VCType] of Message; -- If a message is not processed, it is placed in InBox, blocking that virtual channel
  msg_processed: boolean;
  sharers_copy: multiset [ProcCount] of Node;
  to_be_owner: Node;
  upgraded_to_m: boolean;
  to_be_sharer: Node;
  LastWrite: Value; -- Used to confirm that writes are not lost; this variable would not exist in real hardware

----------------------------------------------------------------------
-- Procedures
----------------------------------------------------------------------
Procedure Send(mtype:MessageType;
	       dst:Node;
	       src:Node;
         vc:VCType;
         val:Value;
         );
var msg:Message;
Begin
  Assert (MultiSetCount(i:Net[dst], true) < NetMax) "Too many messages";
  msg.mtype := mtype;
  msg.src   := src;
  msg.vc    := vc;
  msg.val   := val;
  MultiSetAdd(msg, Net[dst]);
End;

Procedure ErrorUnhandledMsg(msg:Message; n:Node);
Begin
  error "Unhandled message type!";
End;

Procedure ErrorUnhandledState();
Begin
  error "Unhandled state!";
End;

Procedure AddToSharersList(n:Node);
Begin
  if (MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) = 0)
  then
    MultiSetAdd(n, HomeNode.sharers);
  endif;
End;

Function IsSharer(n:Node) : Boolean;
Begin
  return MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) > 0;
End;

Procedure RemoveFromSharersList(n:Node);
Begin
  MultiSetRemovePred(i:HomeNode.sharers, HomeNode.sharers[i] = n);
End;


-- Sends a message to all sharers except rqst
Procedure SendInvReqToSharers(rqst:Node);
Begin
  for n:Node do
    if (IsMember(n, Proc) &
        MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) != 0)
    then
      if n != rqst
      then 
        -- Send invalidation message here
        Send(Inv, n, rqst, VC2, undefined);
      endif;
    endif;
  endfor;
End;

Procedure MultiSetCount_HP(p:Proc; p1:p_state; p2:p_state);
Begin
  -- if (IsUndefined(HomeNode.sharers)) then
  --   Procs[p].state := p1;
  if (MultiSetCount(i:HomeNode.sharers, true) = 0) then
    Procs[p].state := p1;
    upgraded_to_m := false;
    Send(EAck, HomeType, p, VC0, UNDEFINED);
  else
    Procs[p].state := p2;
  endif;
End;

Procedure MultiSetCount_H(h1:h_state; h2:h_state);
Begin
  if (MultiSetCount(i:HomeNode.sharers, true) = 0) then
    HomeNode.state := h1;
  else
    HomeNode.state := h2;
  endif;
End;

Procedure MultiSetCount_H_ADD(h1:h_state; h2:h_state; msg: Message);
Begin
  if (MultiSetCount(i:HomeNode.sharers, true) = 0) then
    HomeNode.state := h1;
  else
    AddToSharersList(msg.src);
    HomeNode.state := h2;
  endif;
End;

-- Check if only 1 sharer
Procedure MultiSetCount_H_PutS(h1:h_state; h2:h_state);
Begin
  if (MultiSetCount(i:HomeNode.sharers, true) = 1)
  then
    HomeNode.state := h1;
  else
    HomeNode.state := h2;
  endif;
End;

Procedure MultiSetCount_P(p:Proc; ps1:p_state; ps2:p_state; msg:Message);
Begin
  if (MultiSetCount(i:sharers_copy, true) = 0)
  then
    Procs[p].state := ps1;
    if (msg.mtype = InvAck | (msg.mtype = GetMAck & MultiSetCount(i:HomeNode.sharers, true) != 0))
    then
      Send(InvAckAll, HomeType, p, VC0, UNDEFINED);
    endif;
  else
    Procs[p].state := ps2;
  endif;
End;

-- Copies all sharers except requestor
Procedure CopySharersList(rqst:Node);
Begin
  for n:Node do
    if (IsMember(n, Proc) &
    MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) != 0)
    then
      if n != rqst
      then
        MultiSetAdd(n, sharers_copy);
      endif;
    endif;
    endfor;

End;

Procedure RemoveFromSharersCopyList(n:Node);
Begin
  if MultiSetCount(i:sharers_copy, sharers_copy[i] = n) > 0
  then
    MultiSetRemovePred(i:sharers_copy, sharers_copy[i] = n);
  endif;
End;



Procedure HomeReceive(msg:Message);
var cnt_sharers:0..ProcCount;  -- for counting sharers
var is_source: 0..ProcCount; 
Begin
-- Debug output may be helpful:
--  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
--  put " at home -- "; put HomeNode.state;

  -- The line below is not needed in Valid/Invalid protocol.  However, the 
  -- compiler barfs if we put this inside a switch, so it is useful to
  -- pre-calculate the sharer count here
  cnt_sharers := MultiSetCount(i:HomeNode.sharers, true);
  is_source := MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = msg.src);


  -- default to 'processing' message.  set to false otherwise
  msg_processed := true;

  switch HomeNode.state
    case H_I:
      switch msg.mtype
        case GetS:
          Send(GetSAck, msg.src, HomeType, VC0, HomeNode.val);
          MultiSetCount_H_ADD(H_IE, H_S, msg);
        case GetM:
          Send(GetMAck, msg.src, HomeType, VC0, HomeNode.val);
          HomeNode.owner := msg.src;
          HomeNode.state := H_M;
        case PutS:
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutM:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutM+data from Owner";
          -- PutMNonOwner
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED); 
        case PutE:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutE+data from Owner";
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED); 
        case PutO:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutO+data from Owner";
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED); 
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;
    
    case H_IE:
      switch msg.mtype
        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false; 
        case PutO:
          msg_processed := false;   
        -- case MAck:
        --   msg_processed := false;      
        case EAck:
          HomeNode.owner := msg.src;
          HomeNode.state := H_E;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;



    case H_S:
      switch msg.mtype
        case GetS:
          Send(GetSAck, msg.src, HomeType, VC0, HomeNode.val);
          AddToSharersList(msg.src);
        case GetM:
          Send(GetMAck, msg.src, HomeType, VC0, HomeNode.val);
          CopySharersList(msg.src);            
          SendInvReqToSharers(msg.src);
          if (cnt_sharers = 0) then
            HomeNode.owner := msg.src;
            HomeNode.state := H_M;
          else
            HomeNode.state := H_SMA;
          endif;
        case PutS:
          -- Send PutAck, check number of sharers to determine if I or S
          if (IsSharer(msg.src)) then
              -- MultiSetCount_H_PutS(H_I, H_S); -- Checks if 1 sharer -> I, else -> S
              RemoveFromSharersList(msg.src);
          endif;
          MultiSetCount_H(H_I, H_S);
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutM:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutM+data from Owner";
          -- PutMNonOwner
          if (IsSharer(msg.src)) then
             -- MultiSetCount_H_PutS(H_I, H_S); 
              RemoveFromSharersList(msg.src);
          endif;
          MultiSetCount_H(H_I, H_S);
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED); 
        case PutE:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutE+data from Owner";
          -- PutENonOwner
          if (IsSharer(msg.src)) then
              RemoveFromSharersList(msg.src);
          endif;
          MultiSetCount_H(H_I, H_S);
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);  
        case PutO:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutO+data from Owner";
          -- PutONonOwner
          if (IsSharer(msg.src)) then
              RemoveFromSharersList(msg.src);
          endif;
          MultiSetCount_H(H_I, H_S);
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);  
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    case H_M:
      Assert (IsUndefined(HomeNode.owner) = false) 
        "HomeNode has no owner, but line is in Modified State";

      switch msg.mtype
        case GetS:
          Send(FwdGetS, HomeNode.owner, msg.src, VC2, UNDEFINED);
          to_be_sharer := msg.src;    
          HomeNode.state := H_MOD; 
        case GetM:
          Send(FwdGetM, HomeNode.owner, msg.src, VC2, UNDEFINED); 
          to_be_owner := msg.src;    -- check
          HomeNode.state := H_MMD;
        case PutS:
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutM:
          if (msg.src = HomeNode.owner) then
            HomeNode.val := msg.val;
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
            undefine HomeNode.owner;
            HomeNode.state := H_I;
          -- PutMNonOwner
          else
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          endif;
        case PutE:
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutO:
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    -- PutO stall???
    case H_E: 
      switch msg.mtype
        case GetS:
          Send(FwdGetS, HomeNode.owner, msg.src, VC2, UNDEFINED);
          to_be_sharer := msg.src;
          if (upgraded_to_m) then
            HomeNode.state := H_MOD;
            upgraded_to_m := false;
          else
            HomeNode.state := H_MSD;
          endif;
          -- if (msg.vc = VC1) then    -- this might cause deadlock
          --   HomeNode.state := H_MSD;
          -- else 
          --   HomeNode.state := H_MOD;
          -- endif;
        case GetM:
          Send(FwdGetM, HomeNode.owner, msg.src, VC2, UNDEFINED); 
          to_be_owner := msg.src;
          HomeNode.state := H_MMD;
        case PutS:
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutM:
          if (msg.src = HomeNode.owner) then
            HomeNode.val := msg.val;
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
            undefine HomeNode.owner;
            HomeNode.state := H_I;
          -- PutMNonOwner
          else
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          endif;
        case PutE:
          if (msg.src = HomeNode.owner) then
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
            undefine HomeNode.owner;
            HomeNode.state := H_I;
          else 
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          endif;
        case PutO:
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          -- msg_processed := false; -- is this correct??
        -- case MAck:
        --   HomeNode.state := H_M;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    -- PutE stall???
    case H_O:
      switch msg.mtype
        case GetS:
          Send(FwdGetS, HomeNode.owner, msg.src, VC2, UNDEFINED);
          HomeNode.state := H_OOD;
          to_be_sharer := msg.src;
        --  AddToSharersList(msg.src);   -- could be wrong not sure if this needs to be done in transient state 
        case GetM:
          if (msg.src = HomeNode.owner) then
            Send(GetMAck, msg.src, HomeType, VC2, UNDEFINED);            
            CopySharersList(msg.src);
            SendInvReqToSharers(msg.src);
            --MultiSetCount_H(H_M, H_SMA);
            HomeNode.state := H_SMA;
          else
            Send(FwdGetM, HomeNode.owner, msg.src, VC2, UNDEFINED);
            CopySharersList(msg.src);
            SendInvReqToSharers(msg.src);
            if (cnt_sharers = 0) then
              HomeNode.state := H_MMD;
              to_be_owner := msg.src;
            else
              HomeNode.state := H_OMA;
              to_be_owner := msg.src;
            endif;
          endif;
        case PutS:
          if (IsSharer(msg.src)) then
              RemoveFromSharersList(msg.src);
          endif;
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutM: --check this not sure if correct
          if (msg.src = HomeNode.owner) then
            HomeNode.val := msg.val;
            undefine HomeNode.owner;
            HomeNode.state := H_S;
          endif;
          if (IsSharer(msg.src)) then
              RemoveFromSharersList(msg.src);
          endif;
          -- MultiSetCount_H(H_I, H_S); could be wrong
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);   
        case PutO:
          if (msg.src = HomeNode.owner) then 
            HomeNode.val := msg.val;
            undefine HomeNode.owner;
            HomeNode.state := H_S;
          else 
            if (IsSharer(msg.src)) then
                RemoveFromSharersList(msg.src);
            endif;
            -- MultiSetCount_H(H_I, H_S);
          endif;
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
        case PutE:
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          --msg_processed := false; -- is this correct?
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;                                          

    case H_OOD:
      switch msg.mtype
        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false; 
        case PutO:
          msg_processed := false;           
        case GetSAck: 
          AddToSharersList(to_be_sharer); 
          undefine to_be_sharer;
          HomeNode.state := H_O; 
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;


    case H_OMA:
      switch msg.mtype
        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false; 
        case PutO:
          msg_processed := false;           
        case InvAckAll: 
          HomeNode.owner := to_be_owner;
          undefine HomeNode.sharers;
          HomeNode.state := H_M;
        case GetMAck:
          HomeNode.state := H_OMA;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;
      
          
    case H_MOD: -- Same as book SD
      switch msg.mtype

        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false; 
        case PutO:
          msg_processed := false; 
        case GetSAck: 
          AddToSharersList(to_be_sharer);
          undefine to_be_sharer;
          HomeNode.val := msg.val;
          HomeNode.state := H_O;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    
    case H_MSD: -- Same as book SD
      switch msg.mtype

        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false; 
        case PutO:
          msg_processed := false; 
        case GetSAck: 
          if (upgraded_to_m) then
            AddToSharersList(to_be_sharer);
            undefine to_be_sharer;
            HomeNode.val := msg.val;
            upgraded_to_m := false;
            HomeNode.state := H_O;
          else
            AddToSharersList(to_be_sharer);
            AddToSharersList(HomeNode.owner);
            undefine to_be_sharer;
            undefine HomeNode.owner;
            HomeNode.val := msg.val;
            HomeNode.state := H_S;
          endif;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    case H_MMD:
      switch msg.mtype
        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false; 
        case PutO:
          msg_processed := false;           
        case GetMAck:
          HomeNode.owner := to_be_owner; 
          HomeNode.state := H_M;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    case H_SMA:
      switch msg.mtype
        case GetS:
          msg_processed := false; 
        case GetM:
          msg_processed := false; 
        case PutS:
          msg_processed := false; 
        case PutM:
          msg_processed := false; 
        case PutE:
          msg_processed := false;
        case PutO:
          msg_processed := false;           
        case InvAckAll:
          undefine HomeNode.sharers;
          HomeNode.owner := msg.src;
          HomeNode.state := H_M;
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;
  endswitch;
End;


Procedure ProcReceive(msg:Message; p:Proc);
var cnt_sharers:0..ProcCount;  -- for counting sharers
Begin
  -- --  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
  -- --  put " at proc "; put p; put "\n";

  --   -- default to 'processing' message.  set to false otherwise
  cnt_sharers := MultiSetCount(i:HomeNode.sharers, true);

  msg_processed := true;

  alias ps:Procs[p].state do
  alias pv:Procs[p].val do

  switch ps
    case P_I:
        ErrorUnhandledMsg(msg, p);

    case P_ISD:
      switch msg.mtype
        case Inv:
          msg_processed := false;
        case GetSAck: 
          pv := msg.val;
          if (msg.src = HomeType) then
            -- if data is exclusively from dir (sharers are undefined/0) then go to Exclusive else go to Shared
            MultiSetCount_HP(p, P_E, P_S);
          else
            ps := P_S;
          endif
        else
            ErrorUnhandledMsg(msg, p);
      endswitch;

    case P_IMAD:
      switch msg.mtype
        case FwdGetS:
          msg_processed := false;
        case FwdGetM:
          msg_processed := false;
        case GetMAck:
          MultiSetCount_P(p, P_M, P_IMA, msg); -- Data from Owner could be problem
          pv := msg.val;
        case InvAck:
          RemoveFromSharersCopyList(msg.src);
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;
    
    case P_IMA:
      switch msg.mtype
        case FwdGetS:
          msg_processed := false;
        case FwdGetM:
          msg_processed := false;
        case InvAck:
          RemoveFromSharersCopyList(msg.src);
          MultiSetCount_P(p, P_M, P_IMA, msg); -- Data from Owner could be problem
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;

    case P_S:
      switch msg.mtype
        case Inv:
          Send(InvAck, msg.src, p, VC0, UNDEFINED);
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;

    case P_SMAD: 
      switch msg.mtype
        case FwdGetS, FwdGetM:
          msg_processed := false;
        case Inv:
          Send(InvAck, msg.src, p, VC0, UNDEFINED);
          ps := P_IMAD;
        case GetMAck:
          MultiSetCount_P(p, P_M, P_SMA, msg); -- Data from Owner could be problem
          pv := msg.val;
        case InvAck:
          RemoveFromSharersCopyList(msg.src);
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;
  
    case P_SMA:
      switch msg.mtype
        case FwdGetS, FwdGetM:
          msg_processed := false;
        case InvAck:
          RemoveFromSharersCopyList(msg.src);
          MultiSetCount_P(p, P_M, P_SMA, msg); -- Data from Owner could be problem
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;

    case P_M:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv);
          ps := P_O;
        case FwdGetM:
          Send(GetMAck, msg.src, p, VC0, pv);
          Send(GetMAck, HomeType, p, VC0, pv);
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;
    
    case P_E:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv); --this may cause deadlock
          ps := P_S;
        case FwdGetM: 
          Send(GetMAck, msg.src, p, VC0, pv);
          Send(GetMAck, HomeType, p, VC0, pv);
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch  

    case P_O:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv);
        case FwdGetM:
          Send(GetMAck, msg.src, p, VC0, pv);
          if (cnt_sharers = 0) then
            Send(GetMAck, HomeType, p, VC0, pv); --cbw
          endif;
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch

    case P_OMAC:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv);
        case FwdGetM:
          Send(GetMAck, msg.src, p, VC0, pv);
          if (cnt_sharers = 0) then
            Send(GetMAck, HomeType, p, VC0, UNDEFINED);
          endif;
          -- Send(GetMAck, HomeType, p, VC0, pv); --wtf
          ps := P_IMAD;
        case GetMAck: --HomeNode needs to make copy of shrares list when it recieves a GetM when in H_O
          MultiSetCount_P(p, P_M, P_OMA, msg);
          if (cnt_sharers = 0) then
            ps := P_M;
            Send(InvAckAll, HomeType, p, VC0, UNDEFINED);
          endif;
          
          -- pv := msg.val;
        case InvAck:
          RemoveFromSharersCopyList(msg.src);
        else
          ErrorUnhandledMsg(msg, p);
      endswitch  

    case P_OMA:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
        case FwdGetM:
          msg_processed := false;
        case InvAck:
          RemoveFromSharersCopyList(msg.src);
          MultiSetCount_P(p, P_M, P_OMA, msg);
        else
          ErrorUnhandledMsg(msg, p);
      endswitch  

    case P_OIA:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv);
        case FwdGetM:
          Send(GetMAck, msg.src, p, VC0, pv);

          if (cnt_sharers = 0) then
            Send(GetMAck, HomeType, p, VC0, pv); --cbw
          endif;
          ps := P_IIA;
        case PutAck:
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch  

    case P_MIA:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv);
          ps := P_OIA;
        case FwdGetM:
          Send(GetMAck, msg.src, p, VC0, pv);
          Send(GetMAck, HomeType, p, VC0, pv);
          ps := P_IIA;
        case PutAck:
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;
    
    case P_EIA:
      switch msg.mtype
        case FwdGetS:
          Send(GetSAck, msg.src, p, VC0, pv);
          Send(GetSAck, HomeType, p, VC0, pv);
          ps := P_SIA;
        case FwdGetM:
          Send(GetMAck, msg.src, p, VC0, pv);
          Send(GetMAck, HomeType, p, VC0, pv);
          ps := P_IIA;
        case PutAck:
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;

    case P_SIA:
      switch msg.mtype
        case Inv:
          Send(InvAck, msg.src, p, VC0, UNDEFINED);
          ps := P_IIA;
        case PutAck:
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;

    case P_IIA:
      switch msg.mtype
        case PutAck:
          undefine pv;
          ps := P_I;
        else
          ErrorUnhandledMsg(msg, p);
      endswitch;
      
    ----------------------------
    -- Error catch
    ----------------------------
    else
      ErrorUnhandledState();

  endswitch;
  
  endalias;
  endalias;
End;

----------------------------------------------------------------------
-- Rules
----------------------------------------------------------------------

-- Processor actions (affecting coherency)

ruleset n:Proc Do
  alias p:Procs[n] Do
-- P_I
  rule "Read request from P_I"
    (p.state = P_I)
  ==>
    Send(GetS, HomeType, n, VC1, UNDEFINED);
    p.state := P_ISD;
  endrule;

  rule "Write request from P_I"
    (p.state = P_I)
  ==>
    Send(GetM, HomeType, n, VC1, UNDEFINED);
    p.state := P_IMAD;
  endrule;


-- P_S
  rule "Store new value - from P_S"
    (p.state = P_S)
      ==>
      Send(GetM, HomeType, n, VC1, UNDEFINED);
      p.state := P_SMAD;
  endrule;

  rule "Replacement - from P_S"
    (p.state = P_S)
    ==>
    Send(PutS, HomeType, n, VC1, p.val); -- Send Processor data for PutS?
    p.state := P_SIA;
  endrule;

----------------------------------------------------------------------
-- P_M
	ruleset v:Value Do
  	rule "Store new value - Only in P_M"
   	 (p.state = P_M)
    	==>
 		   p.val := v;      
 		   LastWrite := v;  --We use LastWrite to sanity check that reads receive the value of the last write
  	endrule;
	endruleset;

  rule "Replacement - from P_M"
    (p.state = P_M)
    ==>
    Send(PutM, HomeType, n, VC1, p.val);
    p.state := P_MIA;
  endrule;



----------------------------------------------------------------------
-- P_E
  rule "Store new value - from P_E"
    (p.state = P_E)
    ==>
    upgraded_to_m := true;
    p.state := P_M;
  endrule;

  rule "Replacement - from P_E"
    (p.state = P_E)
    ==>
    Send(PutE, HomeType, n, VC1, UNDEFINED);
    p.state := P_EIA;
  endrule;
----------------------------------------------------------------------
-- P_O
  rule "Store new value - from P_O"
    (p.state = P_O)
    ==>
    Send(GetM, HomeType, n, VC1, UNDEFINED);
    p.state := P_OMAC;
  endrule;

  rule "Replacement - from P_O"
    (p.state = P_O)
    ==>
    Send(PutO, HomeType, n, VC1, p.val);
    p.state := P_OIA;
  endrule;
----------------------------------------------------------------------
  endalias;
endruleset;

-- Message delivery rules
ruleset n:Node do
  choose midx:Net[n] do
    alias chan:Net[n] do
    alias msg:chan[midx] do
    alias box:InBox[n] do

		-- Pick a random message in the network and delivier it
    rule "receive-net"
			(isundefined(box[msg.vc].mtype))
    ==>

      if IsMember(n, Home)
      then
        HomeReceive(msg);
      else
        ProcReceive(msg, n);
			endif;

			if ! msg_processed
			then
				-- The node refused the message, stick it in the InBox to block the VC.
	  		box[msg.vc] := msg;
			endif;
	  
		  MultiSetRemove(midx, chan);
	  
    endrule;
  
    endalias
    endalias;
    endalias;
  endchoose;  

	-- Try to deliver a message from a blocked VC; perhaps the node can handle it now
	ruleset vc:VCType do
    rule "receive-blocked-vc"
			(! isundefined(InBox[n][vc].mtype))
    ==>
      if IsMember(n, Home)
      then
        HomeReceive(InBox[n][vc]);
      else
        ProcReceive(InBox[n][vc], n);
			endif;

			if msg_processed
			then
				-- Message has been handled, forget it
	  		undefine InBox[n][vc];
			endif;
	  
    endrule;
  endruleset;

endruleset;

----------------------------------------------------------------------
-- Startstate
----------------------------------------------------------------------
startstate

	For v:Value do
  -- home node initialization
  HomeNode.state := H_I;
  undefine HomeNode.sharers;
  undefine HomeNode.owner;
  undefine HomeNode.val;
	endfor;

  upgraded_to_m := false;
  undefine to_be_owner;
  undefine to_be_sharer;
  undefine sharers_copy;
	undefine LastWrite;
  
  -- processor initialization
  for i:Proc do
    Procs[i].state := P_I;
    undefine Procs[i].val;
  endfor;

  -- network initialization
  undefine Net;
  undefine sharers_copy;
endstartstate;

----------------------------------------------------------------------
-- Invariants
----------------------------------------------------------------------

invariant "Invalid implies empty owner"
  HomeNode.state = H_I
    ->
      IsUndefined(HomeNode.owner);

invariant "value is undefined while invalid"
  Forall n : Proc Do	
     Procs[n].state = P_I
    ->
			IsUndefined(Procs[n].val)
	end;


invariant "value in memory matches value of last write, when invalid"
     HomeNode.state = H_I
    ->
			HomeNode.val = LastWrite;

invariant "values in valid state match last write"
  Forall n : Proc Do	
     Procs[n].state = P_M | Procs[n].state = P_E
    ->
			Procs[n].val = LastWrite --LastWrite is updated whenever a new value is created 
	end;
	

	
	
-- Here are some invariants that are helpful for validating shared state.

invariant "modified and exclusive implies empty sharers list"
  HomeNode.state = H_M | HomeNode.state = H_E
    ->
      MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "Invalid implies empty sharer list"
  HomeNode.state = H_I
    ->
      MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "values in shared state match memory"
  Forall n : Proc Do	
     HomeNode.state = H_S & Procs[n].state = P_S
    ->
			HomeNode.val = Procs[n].val
	end;


invariant "values in memory matches value of last write, when shared or invalid"
  Forall n : Proc Do	
     HomeNode.state = H_S | HomeNode.state = H_I 
    ->
			HomeNode.val = LastWrite
	end;


