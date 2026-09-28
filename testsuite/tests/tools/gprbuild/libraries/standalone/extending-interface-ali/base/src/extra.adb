package body Extra is
   function Twice (X : Defs.Level) return String is
   begin
      return Defs.Level'Image (X) & Defs.Level'Image (X);
   end Twice;
end Extra;
