with GPR2; use GPR2;
with GPR2.Context;
with GPR2.KB;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Main is
   Base : GPR2.KB.Object := GPR2.KB.Create_Default (GPR2.KB.Default_Flags);
   PT   : GPR2.Project.Tree.Object;

   Error_Parsing_CGPR : Boolean := False;
begin
   Base.Add (GPR2.KB.Default_Flags, GPR2.Path_Name.Create_File ("foo.xml"));

   PT.Load_Autoconf
     (GPR2.Path_Name.Create_File ("a.gpr"),
      GPR2.Context.Empty,
      Base => Base);

   Ada.Text_IO.Put_Line ("Invalid KB chunk ignored");

exception

   when others =>
      --  Cant's be quite sure on the exact error message so we just check that
      --  the error relates to an autoconf sloc.
      for M of PT.Log_Messages.all loop
         if Ada.Strings.Fixed.Head (M.Format, 14) = "autoconf.cgpr:" then
            Error_Parsing_CGPR := True;
         end if;
      end loop;

      if not Error_Parsing_CGPR then
         Ada.Text_IO.Put_Line ("Error in autoconf.cgpr not reported");
      end if;
end Main;
