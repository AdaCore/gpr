--  Checks that an action belonging to an externally built view is always
--  reported as No_Op, even when the build explicitly deactivates it: such
--  an action has nothing to do, but must not hold its successors back.

with Ada.Text_IO;

with GPR2.Build.Actions;
with GPR2.Build.Actions_Population;
with GPR2.Build.Options;
with GPR2.Build.Tree_Db;
with GPR2.Options;
with GPR2.Project.Tree;

use GPR2;
use GPR2.Build;

function Main return Natural is
   package Holders renames Actions.Action_Id_Holder;

   Tree     : GPR2.Project.Tree.Object;
   Opts     : GPR2.Options.Object;
   B_Opts   : Build.Options.Build_Options;

   Ext_Id   : Holders.Holder;
   Local_Id : Holders.Holder;

   procedure Check (Label : String; Id : Holders.Holder);
   --  Deactivate the action and report the state on both sides of the call

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; Id : Holders.Holder) is
   begin
      if Id.Is_Empty then
         Ada.Text_IO.Put_Line (Label & ": no such action");
         return;
      end if;

      declare
         Db  : constant Build.Tree_Db.Object_Access :=
                 Tree.Artifacts_Database;
         Ref : constant Build.Tree_Db.Action_Reference_Type :=
                 Db.all.Action_Id_To_Reference (Id.Element);
      begin
         Ada.Text_IO.Put (Label & ": before " & Ref.State'Image);
         Ref.Set_State (Actions.Deactivated);
         Ada.Text_IO.Put_Line (", after Set_State (Deactivated) "
                               & Ref.State'Image);
      end;
   end Check;

begin
   Opts.Add_Switch (GPR2.Options.P, "tree/main.gpr");
   Opts.Add_Switch (GPR2.Options.X, "EXT_BUILT=true");

   if not Tree.Load (Opts) then
      return 1;
   end if;

   Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts);

   if not Build.Actions_Population.Populate_Actions
     (Tree, B_Opts, With_Externally_Built => True)
   then
      Ada.Text_IO.Put_Line ("population failed");
      return 1;
   end if;

   --  Pick one action on each side of the externally built frontier

   for A of Tree.Artifacts_Database.All_Actions loop
      if A.View.Is_Externally_Built then
         if Ext_Id.Is_Empty then
            Ext_Id := Holders.To_Holder (A.UID);
         end if;
      elsif Local_Id.Is_Empty then
         Local_Id := Holders.To_Holder (A.UID);
      end if;
   end loop;

   Check ("externally built", Ext_Id);
   Check ("regular view     ", Local_Id);

   return 0;
end Main;
