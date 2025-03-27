
-- two-state 4-hop VI protocol

----------------------------------------------------------------------
-- Constants,
----------------------------------------------------------------------
const
  ProcCount: 3;          -- number processors
  ValueCount:   2;       -- number of data values.
  VC0: 0;                -- Data, InvAck, InvAckAll, GetMAck, GetSAck - Low Priority
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

  h_state: enum {H_I, H_S, H_M, -- stable states
                 H_MSD, H_MMD, H_SMA -- transient states
                };
  p_state: enum { P_I, P_S, P_M, -- stable states
                  P_ISD, P_IMAD, P_IMA, P_SMAD, P_SMA, P_MIA, P_SIA, P_IIA --transient states
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
                     PutS,
                     PutAck,
                     InvAckAll
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

Procedure MultiSetCount_H(h1:h_state; h2:h_state);
Begin
  if (MultiSetCount(i:HomeNode.sharers, true) = 0)
  then
    HomeNode.state := h1;
  else
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
    if (msg.mtype = InvAck | (msg.mtype = GetMAck & MultiSetCount(i:HomeNode.sharers, true) != 0)) --& msg.vc = VC0))
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

-- Parin
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
          AddToSharersList(msg.src);
          HomeNode.state := H_S;

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
        else
          ErrorUnhandledMsg(msg, HomeType);
      endswitch;

    case H_S:
      -- CHECK: Assertion is valid, make sure nonzero sharers if in H_S
      switch msg.mtype
        -- CHECK: case PutM - Can't get here?

        case GetS:
          Send(GetSAck, msg.src, HomeType, VC0, HomeNode.val);
          AddToSharersList(msg.src);

        case GetM:
        -- Send Invalidation request to sharers
          -- CHECK: What are we waiting for? I think wait for InvAckAll
          -- if (is_source = 1 & cnt_sharers = 1) then     
          --   Send(GetMAck, msg.src, HomeType, VC0, HomeNode.val);
          --   HomeNode.owner := msg.src;
          --   HomeNode.state := H_M;
          --   undefine HomeNode.sharers;
          -- else

            Send(GetMAck, msg.src, HomeType, VC0, HomeNode.val);
            CopySharersList(msg.src);            
            SendInvReqToSharers(msg.src);
            -- MultiSetCount_H(H_MMD, H_SMA);
            HomeNode.state := H_SMA;
          --endif;

        case PutS:
          -- Send PutAck, check number of sharers to determine if I or S
          if (IsSharer(msg.src)) then
              MultiSetCount_H_PutS(H_I, H_S); -- Checks if 1 sharer -> I, else -> S
              RemoveFromSharersList(msg.src);
          endif;

          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);

        case PutM:
          Assert (msg.src != HomeNode.owner) 
          "Can't receive PutM+data from Owner";
          -- PutMNonOwner
          if (IsSharer(msg.src)) then
              MultiSetCount_H_PutS(H_I, H_S); -- Checks if 1 sharer -> I, else -> S
              RemoveFromSharersList(msg.src);
          endif;
          -- MultiSetCount_H_PutS(H_I, H_S); -- Checks if 1 sharer -> I, else -> S
          -- RemoveFromSharersList(msg.src);
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
          to_be_sharer := msg.src;    -- check\
          -- AddToSharersList(to_be_sharer);
          -- AddToSharersList(HomeNode.owner);
          -- undefine to_be_sharer;
          -- undefine HomeNode.owner;
          HomeNode.state := H_MSD; 

        case GetM:
          Send(FwdGetM, HomeNode.owner, msg.src, VC2, UNDEFINED); -- here msg.src  is ssrc instead of HomeType, so that the GetMAck can be                                                                  -- sent to the requesting proc
          to_be_owner := msg.src;    -- check
          HomeNode.state := H_MMD;

        case PutS:
          Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);

        case PutM:
          -- assert (msg.src = HomeNode.owner) "Writeback from non-owner";
          if (msg.src = HomeNode.owner) then
            HomeNode.val := msg.val;
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
            undefine HomeNode.owner;
            HomeNode.state := H_I;
          -- PutMNonOwner
          else
            Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          endif;


        else
          ErrorUnhandledMsg(msg, HomeType);

      endswitch;

    
    case H_MSD: -- Same as book SD
      switch msg.mtype

        case GetS:
          msg_processed := false; -- stall message in InBox
      
        case GetM:
          msg_processed := false; -- stall message in InBox

        case PutS:
          -- RemoveFromSharersList(msg.src);
          -- Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          msg_processed := false; -- stall message in InBox

        case PutM:
          -- Assert (msg.src != HomeNode.owner) 
          -- "Can't receive PutM+data from Owner";
          -- -- PutMNonOwner
          -- RemoveFromSharersList(msg.src);
          -- Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);

          msg_processed := false; -- stall message in InBox


        case GetSAck: -- Comes from Processor?
          AddToSharersList(to_be_sharer);
          AddToSharersList(HomeNode.owner);
          undefine to_be_sharer;
          undefine HomeNode.owner;
          HomeNode.val := msg.val;
          HomeNode.state := H_S;

        else
          ErrorUnhandledMsg(msg, HomeType);

      endswitch;

    case H_MMD:
      switch msg.mtype
        
        case GetS:
          msg_processed := false; -- stall message in InBox

        case GetM:
          msg_processed := false; -- stall message in InBox

        case PutS:
          -- Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          msg_processed := false; -- stall message in InBox

        case PutM:
          -- Assert (msg.mtype != PutM)
          -- "Can't recieve PutM while in H_MMD";
          msg_processed := false; -- stall message in InBox

        case GetMAck:
          HomeNode.owner := to_be_owner; -- Check: better to put in as msg.src like two state?
          HomeNode.state := H_M;

        else
          ErrorUnhandledMsg(msg, HomeType);

      endswitch;


    case H_SMA:
      -- if (MultiSetCount_H(H_M, H_SMA))
      switch msg.mtype

        case GetS:
          msg_processed := false; -- stall message in InBox

        case GetM:
          msg_processed := false; -- stall message in InBox

        case PutS:
          -- Send(PutAck, msg.src, HomeType, VC2, UNDEFINED);
          msg_processed := false; -- stall message in InBox

        case PutM:
          -- msg_processed := false; -- stall message in InBox
          msg_processed := false; -- stall message in InBox

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
Begin
-- --  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
-- --  put " at proc "; put p; put "\n";

--   -- default to 'processing' message.  set to false otherwise
  msg_processed := true;

  alias ps:Procs[p].state do
  alias pv:Procs[p].val do

  switch ps
    case P_I:
        ErrorUnhandledMsg(msg, p);

    case P_ISD:
      if (msg.mtype = Inv) then
          msg_processed := false;

      elsif (msg.mtype = GetSAck) then
          ps := P_S;

      else
          ErrorUnhandledMsg(msg, p);
      endif;

    case P_IMAD:

      if (msg.mtype = FwdGetS | msg.mtype = FwdGetM) then
          msg_processed := false;

      elsif (msg.mtype = GetMAck) then
          MultiSetCount_P(p, P_M, P_IMA, msg); -- Data from Owner could be problem

      elsif (msg.mtype = InvAck) then
        RemoveFromSharersCopyList(msg.src)

      else
          ErrorUnhandledMsg(msg, p);
      endif;
    
    case P_IMA:
      if (msg.mtype = FwdGetS | msg.mtype = FwdGetM) then
          msg_processed := false;

      elsif (msg.mtype = InvAck) then

        RemoveFromSharersCopyList(msg.src);
        MultiSetCount_P(p, P_M, P_IMA, msg); -- Data from Owner could be problem

      else
          ErrorUnhandledMsg(msg, p);
      endif;

    case P_S:
      if (msg.mtype = Inv) then
          Send(InvAck, msg.src, p, VC0, UNDEFINED);
          ps := P_I;

      else
          ErrorUnhandledMsg(msg, p);
      endif;

    case P_SMAD: 
     
      if (msg.mtype = FwdGetS | msg.mtype = FwdGetM) then -- Check if stall for GetSack
        msg_processed := false;

      elsif (msg.mtype = Inv) then
        Send(InvAck, msg.src, p, VC0, UNDEFINED);
        ps := P_IMAD;

      elsif (msg.mtype = GetMAck) then
        MultiSetCount_P(p, P_M, P_SMA, msg); -- Data from Owner could be problem

      elsif (msg.mtype = InvAck) then
        RemoveFromSharersCopyList(msg.src);

      else
        ErrorUnhandledMsg(msg, p);
      endif;
  
    case P_SMA:
      if (msg.mtype = FwdGetS | msg.mtype = FwdGetM) then
          msg_processed := false;

      elsif (msg.mtype = InvAck) then
        RemoveFromSharersCopyList(msg.src);
        MultiSetCount_P(p, P_M, P_SMA, msg); -- Data from Owner could be problem

      else
          ErrorUnhandledMsg(msg, p);
      endif;

    case P_M:
      if (msg.mtype = FwdGetS) then
        Send(GetSAck, msg.src, p, VC0, pv);
        Send(GetSAck, HomeType, p, VC0, pv);
        ps := P_S;

      elsif (msg.mtype = FwdGetM) then
        Send(GetMAck, msg.src, p, VC0, pv);
        Send(GetMAck, HomeType, p, VC0, pv);
        ps := P_I;
      
      else
        ErrorUnhandledMsg(msg, p);
      endif;

    case P_MIA:
      if (msg.mtype = FwdGetS) then
        Send(GetSAck, msg.src, p, VC0, pv);
        Send(GetSAck, HomeType, p, VC0, pv);
        ps := P_SIA;

      elsif (msg.mtype = FwdGetM) then
        Send(GetMAck, msg.src, p, VC0, pv);
        Send(GetMAck, HomeType, p, VC0, pv);
        ps := P_IIA;

      elsif (msg.mtype = PutAck) then
        ps := P_I;

      else
        ErrorUnhandledMsg(msg, p);
      endif;

    case P_SIA:
      if (msg.mtype = Inv) then
        Send(InvAck, msg.src, p, VC0, UNDEFINED);
        ps := P_IIA;

      elsif (msg.mtype = PutAck) then
        ps := P_I;

      else
        ErrorUnhandledMsg(msg, p);
      endif;
      
    case P_IIA:
      if (msg.mtype = PutAck) then
        ps := P_I;

      else
        ErrorUnhandledMsg(msg, p);
      endif;
      
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
    Send(PutM, HomeType, n, VC1, p.val); -- Send Processor data for PutM?
    p.state := P_MIA;
  endrule;

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
  HomeNode.val := v;
	endfor;
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

-- invariant "Invalid implies empty owner"
--   HomeNode.state = H_I
--     ->
--       IsUndefined(HomeNode.owner);

-- invariant "value in memory matches value of last write, when invalid"
--      HomeNode.state = H_I
--     ->
-- 			HomeNode.val = LastWrite;

-- invariant "values in valid state match last write"
--   Forall n : Proc Do	
--      Procs[n].state = P_M
--     ->
-- 			Procs[n].val = LastWrite --LastWrite is updated whenever a new value is created 
-- 	end;
	
-- invariant "value is undefined while invalid"
--   Forall n : Proc Do	
--      Procs[n].state = P_I
--     ->
-- 			IsUndefined(Procs[n].val)
-- 	end;
	
/*	
-- Here are some invariants that are helpful for validating shared state.

invariant "modified implies empty sharers list"
  HomeNode.state = H_Modified
    ->
      MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "Invalid implies empty sharer list"
  HomeNode.state = H_Invalid
    ->
      MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "values in memory matches value of last write, when shared or invalid"
  Forall n : Proc Do	
     HomeNode.state = H_Shared | HomeNode.state = H_Invalid
    ->
			HomeNode.val = LastWrite
	end;

invariant "values in shared state match memory"
  Forall n : Proc Do	
     HomeNode.state = H_Shared & Procs[n].state = P_Shared
    ->
			HomeNode.val = Procs[n].val
	end;
*/	
