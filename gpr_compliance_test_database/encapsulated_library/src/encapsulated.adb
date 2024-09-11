package body encapsulated is
   function Msg return String is
   begin
      return raise Program_Error with "Encapsulated.Msg";
   end Msg;
end encapsulated;
