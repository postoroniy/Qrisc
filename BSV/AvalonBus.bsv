

package AvalonBus;


import FIFO::*;
import FIFOF::*;
import FIFOLevel::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;

// Type for avalon bus data
typedef UInt#(32) AvalonWordT;
typedef Maybe#(AvalonWordT) ReturnedDataT;

// Memory access type.  Note that MemNull used as part of arbiterlock release message.
typedef enum { MemRead, MemWrite, MemNull } MemAccessT deriving(Bits,Eq);

// Structure for memory requests
typedef struct {
   MemAccessT   rw;
   UInt#(word_address_width)  addr; // word address
   AvalonWordT  data;
   Bool arbiterlock;
} MemAccessPacketT#(numeric type word_address_width) deriving(Bits,Eq);

(* always_ready, always_enabled *)
interface AvalonSlaveIfc#(numeric type word_address_width);
   method Action s0(UInt#(word_address_width) address, AvalonWordT writedata,
            Bool write, Bool read, Bool arbiterlock); //, Bool resetrequest);
   method AvalonWordT s0_readdata;
   method Bool s0_waitrequest;
endinterface

(* always_ready, always_enabled *)
interface AvalonMasterIfc#(numeric type word_address_width);
   method Action m0(AvalonWordT readdata, Bool waitrequest);
   method AvalonWordT m0_writedata;
   method UInt#(TAdd#(2,word_address_width)) m0_address;
   method Bool m0_read;
   method Bool m0_write;
   method Bool m0_arbiterlock;
endinterface

interface AvalonBridgeIfc#(numeric type word_address_width);
  interface AvalonSlaveIfc#(word_address_width) avs;
  interface AvalonMasterIfc#(word_address_width) avm;
endinterface

interface AvalonSlave2ClientIfc#(numeric type word_address_width);
  interface AvalonSlaveIfc#(word_address_width) avs;
  interface Client#(MemAccessPacketT#(word_address_width),ReturnedDataT) client;
//(* always_read, always_enabled *)  method Bool reset_from_bus;
endinterface

interface Server2AvalonMasterIfc#(numeric type word_address_width);
  interface AvalonMasterIfc#(word_address_width) avm;
  interface Server#(MemAccessPacketT#(word_address_width),ReturnedDataT) server;
endinterface

module mkAvalonSlave2Client(AvalonSlave2ClientIfc#(word_address_width))
   provisos(Max#(word_address_width,29,29));

   // bypass wires for incoming Avalon slave signals
   Wire#(UInt#(word_address_width)) address_w   <- mkBypassWire;
   Wire#(AvalonWordT) writedata_w          <- mkBypassWire;
   Wire#(Bool)        read_w               <- mkBypassWire;
   Wire#(Bool)        write_w              <- mkBypassWire;
   Wire#(Bool)        arbiterlock_w        <- mkBypassWire;
   Reg# (Bool)        prev_arbiterlock     <- mkReg(False);
//   Wire#(Bool)        resetrequest_w       <- mkBypassWire;

   // bypass wire for Avalon wait signal + pulsewires to clear
   Wire#(Bool)        avalonwait           <- mkBypassWire;
   PulseWire          avalonwait_end_read  <- mkPulseWire;
   PulseWire          avalonwait_end_write <- mkPulseWire;

   // DWire for read data returned to Avalon slave bus
   Wire#(AvalonWordT) datareturned <- mkDWire(32'hdeaddead);

   // reg indicating that the Avalon request is being processed and further
   // requests should be ignored until the avalonwait signal has been released
   // (gone low)
   Reg#(Bool) ignore_further_requests <- mkReg(False);

   // FIFO holding requests received from Avalon slave bus sent out via
   // the client request interface
   FIFOF#(MemAccessPacketT#(word_address_width)) outbuf <- mkFIFOF;

   // provide the avalonwait signal
   // note: this must appear within the same clock cycle that a read or write
   //       is initiated
   (* no_implicit_conditions *)
   rule wire_up_avalonwait;
      avalonwait <= (read_w && !avalonwait_end_read) || (write_w && !avalonwait_end_write);
   endrule

   rule arbiterlock_history;
      prev_arbiterlock <= arbiterlock_w;
   endrule

   rule handle_end_arbiterlock (prev_arbiterlock && !arbiterlock_w && !read_w && !write_w);
      outbuf.enq(MemAccessPacketT{
                     rw:   MemNull,     // send MemNull to clear arbiter lock
                     addr: address_w,   // don't care what the address and data are but keep...
                     data: writedata_w, // ...consistent with next rule to simplify implementation
                     arbiterlock: arbiterlock_w});
   endrule

   // if this is a new Avalon slave bus request then enqueue
   // note: if outbuf FIFO is full, Avalon slave forced to wait
   rule hanlde_bus_requests ((read_w || write_w) && !ignore_further_requests);
      outbuf.enq(MemAccessPacketT{
                   rw: read_w ? MemRead : MemWrite,
                   addr: address_w,
                   data: writedata_w, // N.B. "data" is undefined for reads
                   arbiterlock: arbiterlock_w});
      ignore_further_requests <= read_w;
      // release avalonwait for writes since the request has been enqueued
      if(write_w) avalonwait_end_write.send;
   endrule

   // once avalonwait has gone low, get ready to respond to next request
   // from the Avalon bus
   rule cancel_ingore_further_requests(!avalonwait && ignore_further_requests);
      ignore_further_requests <= False;
   endrule

   // Avalon slave interface - just wiring
   interface AvalonSlaveIfc avs;
      method Action s0(address, writedata, write, read, arbiterlock); //, resetrequest);
           address_w     <= address;
           writedata_w   <= writedata;
           write_w       <= write;
           read_w        <= read;
           arbiterlock_w <= arbiterlock;
      //     resetrequest_w <= resetrequest;
      endmethod

      method s0_readdata;
         return datareturned;
      endmethod

      method s0_waitrequest;
         return avalonwait;
      endmethod
   endinterface

   // client interface
   interface Client client;
      interface request = toGet(outbuf);
      interface Put response;
        method Action put(d);
          // note: respond to data read
          // currently if d is Invalid then ignored but it could be used
          // to do a avalonwait_end_write.send if it was required the
          // clients waited on writes until the writes had completed
          if(isValid(d))
          begin
            // note duality of DWire for data and PulseWire for
            //  associated signal
            datareturned <= fromMaybe(32'hdeaddead,d);
            avalonwait_end_read.send;
          end
        endmethod
      endinterface
   endinterface

//   method Bool reset_from_bus;
//      return resetrequest_w;
//   endmethod
endmodule

module mkServer2AvalonMaster(Server2AvalonMasterIfc#(word_address_width))
   provisos(Max#(word_address_width,29,29),
        Add#(word_address_width, 2, TAdd#(2, word_address_width)));
   // bypass wires for incoming Avalon master signals
   // N.B. avalon master address is a byte address, so need to add 2 bits
   Reg#(UInt#(word_address_width))  address_r       <- mkReg(0);
   Reg#(AvalonWordT)  writedata_r     <- mkReg(0);
   Reg#(Bool)         read_r          <- mkReg(False);
   Reg#(Bool)         write_r         <- mkReg(False);
   Reg#(Bool)         arbiterlock_r   <- mkReg(False);
   PulseWire          signal_read     <- mkPulseWire;
   PulseWire          signal_write    <- mkPulseWire;
   Wire#(Bool)        avalonwait      <- mkBypassWire;
   Wire#(AvalonWordT) avalonreaddata  <- mkBypassWire;

   // buffer data returned
   // TODO: could this buffer be removed by not initiating the transaction
   // until the returndata get operation was active, then do the memory
   // transaction and return the value to the get without buffering?
   //  - possibly not if the interface is fully pipelined because there
   //    can be several transactions ongoing (several addresses issued, etc.)
   //    before data comes back

   // FIFO of length 4 which is:
   // Unguarded enq since it it guarded by the bus transaction initiation
   // Guarded deq
   // Unguarded count so isLessThan will not block
   FIFOLevelIfc#(ReturnedDataT,4) datareturnbuf <- mkGFIFOLevel(True,False,True);
   FIFO#(MemAccessT) pending_acks <- mkFIFO;

   let write_ack = write_r && !read_r && !avalonwait;
   let read_ack  = !write_r && read_r && !avalonwait;

   rule buffer_data_read (read_ack && (pending_acks.first==MemRead));
      datareturnbuf.enq(tagged Valid avalonreaddata);
      $display("   %05t: Avalon2ClientServer returning data",$time);
      pending_acks.deq;
   endrule

   rule signal_data_write (write_ack && (pending_acks.first==MemWrite));
      datareturnbuf.enq(tagged Invalid); // signal write has happened
      pending_acks.deq;
   endrule

   rule signal_mem_null (pending_acks.first==MemNull);
      datareturnbuf.enq(tagged Invalid); // signal null has happened
      pending_acks.deq;
   endrule

   (* no_implicit_conditions *)
   rule do_read_reg;
      if(signal_read) read_r <= True;
      else if(!avalonwait) read_r <= False;
   endrule

   (* no_implicit_conditions *)
   rule do_write_reg;
      if(signal_write) write_r <= True;
      else if(!avalonwait) write_r <= False;
   endrule

   // Avalon master interface - just wiring
   interface AvalonMasterIfc avm;
      method Action m0(readdata, waitrequest);
        avalonreaddata <= readdata;
        avalonwait <= waitrequest;
      endmethod

      method m0_writedata;   return writedata_r;    endmethod
      method m0_address;     return unpack({pack(address_r),2'b00});   endmethod
      method m0_read;        return read_r;         endmethod
      method m0_write;       return write_r;        endmethod
      method m0_arbiterlock; return arbiterlock_r;  endmethod
   endinterface

   // server interface
   interface Server server;
      interface response = toGet(datareturnbuf);
      interface Put request;
         method Action put(packet) if (!avalonwait && datareturnbuf.isLessThan(2));
            address_r     <= packet.addr;
            writedata_r   <= packet.data;
            arbiterlock_r <= packet.arbiterlock;
            pending_acks.enq(packet.rw);
            case(packet.rw)
               MemRead:  signal_read.send();
               MemWrite: signal_write.send();
            endcase
         endmethod
      endinterface
   endinterface

endmodule

module mkAvalonBridge(AvalonBridgeIfc#(word_address_width))
   provisos(Max#(word_address_width,29,29),
        Add#(word_address_width, 2, TAdd#(2, word_address_width)));

   AvalonSlave2ClientIfc#(word_address_width) client <- mkAvalonSlave2Client;
   Server2AvalonMasterIfc#(word_address_width) server <- mkServer2AvalonMaster;

   mkConnection(client.client,server.server);

   interface avs = client.avs;
   interface avm = server.avm;
endmodule

endpackage
