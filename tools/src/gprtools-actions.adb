with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Ada_Compile;
with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Unit_Info.List;

with GPR2.Path_Name;
with GPR2.Project.View;

use GPR2;

package body GPRtools.Actions is

   function Add_Actions_To_Build_Mains
     (Tree : GPR2.Project.Tree.Object; Log : out GPR2.Log.Object)
      return Boolean
   is

      package GBA renames GPR2.Build.Actions;

      function Add_Actions_To_Build_Main
        (Main : GPR2.Build.Compilation_Unit.Unit_Location) return Boolean;

      -------------------------------
      -- Add_Actions_To_Build_Main --
      -------------------------------

      function Add_Actions_To_Build_Main
        (Main : GPR2.Build.Compilation_Unit.Unit_Location) return Boolean
      is
         A      : GBA.Ada_Compile.Object;
         Source : constant GPR2.Build.Source.Object :=
                    Main.View.Source (Main.Source.Simple_Name);

      begin
         A.Initialize
           (Main.View.Unit
              (Source.Units.Element (No_Index).Name));

         if not Tree.Artifacts_Database.Has_Action (A.UID) then
            Tree.Artifacts_Database.Add_Action (A, Log);

            if Log.Has_Error then
               return False;
            end if;
         end if;

         declare
            Bind_Action : GBA.Ada_Bind.Object;
            PB_Id       : GBA.Post_Bind.Post_Bind_Id;
         begin
            Bind_Action.Initialize (A.Ali_File, Main.View);
            Tree.Artifacts_Database.Add_Action (Bind_Action, Log);

            if Log.Has_Error then
               return False;
            end if;

            PB_Id := GBA.Post_Bind.Post_Bind_Id
              (Bind_Action.Post_Bind_Action);

            declare
               L  : GBA.Link.Object;
               PB : constant GBA.Post_Bind.Object :=
                      GBA.Post_Bind.Object
                        (Tree.Artifacts_Database.Action (PB_Id));
            begin
               L.Initialize
                 (Main.View.Executable (Main.Source.Simple_Name, Main.Index),
                  A.Object_File, Main.View);

               L.Add_Object_File (PB.Object_File);

               Tree.Artifacts_Database.Add_Action (L, Log);

               if Log.Has_Error then
                  return False;
               end if;
            end;
         end;

         return True;
      end Add_Actions_To_Build_Main;

   begin
      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            if not Add_Actions_To_Build_Main (Main) then
               return False;
            end if;
         end loop;

         --  ??? Useless sources may be compiled here. Need to find a better
         --  way to process non-Ada sources.

         for Src of Root.Sources (Compilable_Only => True) loop
            if Src.Language /= Ada_Language and then Src.Kind = S_Body then
               declare
                  A : GBA.Compile.Object;
               begin
                  A.Initialize (Src);

                  if not Tree.Artifacts_Database.Has_Action (A.UID) then
                     Tree.Artifacts_Database.Add_Action (A, Log);
                     if Log.Has_Error then
                        return False;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         Tree.Artifacts_Database.Propagate_Actions;
      end loop;

      return True;
   end Add_Actions_To_Build_Mains;
end GPRtools.Actions;
