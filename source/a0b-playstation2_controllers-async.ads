--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Asynchronous driver of the PlayStation2 controller.

with Ada.Synchronous_Task_Control;

package A0B.PlayStation2_Controllers.Async is

   package Communication is

      type Status is record
         Last            : A0B.Types.Unsigned_8;
         Not_Acknoledged : Boolean;
         --  Second data byte of the packet contains encoded length of the
         --  data. All bytes except the last must be acknoledged by the
         --  controller. This flag is set on timeout of acknoledge awaiting,
         --  and means that transmittion is incomplete. Some typical cases:
         --    * controller is not connected (no acknoledge for the first byte)
         --    * controller doesn't support command (no acknoledge for the
         --      second byte)
      end record;

      --  type Status_Access is access all Status with Storage_Size => 0;

      type Buffer is
        array (A0B.Types.Unsigned_8 range 0 .. 31) of A0B.Types.Unsigned_8;
      --  Type for the internal buffer.

      --  type Buffer_Access is access all Buffer with Storage_Size => 0;

      --  type Suspension_Object_Access is
      --    access all Ada.Synchronous_Task_Control.Suspension_Object
      --      with Storage_Size => 0;

      type Abstract_Communication_Channel is limited interface;

--     not overriding procedure Exchange
--       (Self   : in out Communication_Driver;
--        Input  : Stream_Element_Array;
--        Output : out Stream_Element_Array;
--        Last   : out Stream_Element_Offset;
--        Status : out Communication_Status) is abstract;

      not overriding procedure Initialize
        (Self   : in out Abstract_Communication_Channel) is abstract;
        --   Input  : not null access constant Buffer;
        --   Output : not null access Buffer) is abstract;

      not overriding procedure Finalize
        (Self : in out Abstract_Communication_Channel) is abstract;

      not overriding procedure Exchange
        (Self   : in out Abstract_Communication_Channel;
         Buffer : aliased in out Communication.Buffer;
         Status : aliased out Communication.Status) is abstract;
        --   Buffer : Communication.Buffer_Access;
        --   Status : Communication.Status_Access;
        --   Done   : Communication.Suspension_Object_Access) is abstract;
      --  Exchange data with controller. It is blocking operation.

   end Communication;

   type PlayStation2_Controller
     (Channel :
        not null access Communication.Abstract_Communication_Channel'Class)
     is tagged limited private;

   type Polling_Rate is range 1 .. 1_000;

   procedure Initialize
     (Self : in out PlayStation2_Controller'Class);

   procedure Configure
     (Self             : in out PlayStation2_Controller'Class;
      Analog_Joysticks : Boolean;
      Analog_Buttons   : Boolean      := False;
      Rate             : Polling_Rate := 250);

   procedure Get_State
     (Self  : PlayStation2_Controller'Class;
      State : out Controller_State);

   procedure Finalize (Self : in out PlayStation2_Controller'Class);

private

   type Buffer_Index is mod 2;

   type Buffer_Storage is
     array (Buffer_Index) of aliased Communication.Buffer;

   type Configuration is record
      Rate             : Polling_Rate := 250;
      Analog_Joysticks : Boolean      := False;
      Analog_Buttons   : Boolean      := False;
   end record with Pack, Object_Size => 32;

   type Configuration_Index is mod 2;

   type Configuration_Array is array (Configuration_Index) of Configuration
     with Atomic_Components;

   task type Driver (Self : not null access PlayStation2_Controller'Class)
     with Priority => 200;

   type PlayStation2_Controller
     (Channel :
        not null access Communication.Abstract_Communication_Channel'Class)
        is tagged limited
   record
      Buffer               : Buffer_Storage;
      --  IO buffers.

      Communication_Buffer : Buffer_Index := 0;
      --  Index of the buffer might be used for IO operations. Another buffer
      --  is used to store raw state data received previously.

      Configuration        : Configuration_Array;
      Active_Configuration : Configuration_Index := 0
        with Atomic, Volatile;
      --  Active and requested configuration.

      Change_Configuration : Boolean with Atomic, Volatile;
      --  Request to change controller's configuration.

      Driver_Enabled       : Boolean := False with Volatile;
      Driver_Suspension    : Ada.Synchronous_Task_Control.Suspension_Object;
      Driver_Task          : Driver (PlayStation2_Controller'Unchecked_Access);
   end record;

   function Payload_Length
     (Buffer : Communication.Buffer) return A0B.Types.Unsigned_8 with Inline;
   --  Return payload length in 2-byte words.

end A0B.PlayStation2_Controllers.Async;