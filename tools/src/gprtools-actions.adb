with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;
pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Build.Tree_Db;

with GPR2.Path_Name;
with GPR2.Project.View;

use GPR2, GPR2.Build;

package body GPRtools.Actions is

   function Add_Actions_To_Build_Mains
     (Tree : GPR2.Project.Tree.Object) return Boolean
   is

      package GBA renames GPR2.Build.Actions;

      function Add_Actions_To_Build_Lib
        (View : GPR2.Project.View.Object) return Boolean;

      function Add_Actions_To_Build_Main
        (Main : GPR2.Build.Compilation_Unit.Unit_Location) return Boolean;

      Tree_Db : GPR2.Build.Tree_Db.Object_Access renames
                  Tree.Artifacts_Database;

      ------------------------------
      -- Add_Actions_To_Build_Lib --
      ------------------------------

      function Add_Actions_To_Build_Lib
        (View : GPR2.Project.View.Object) return Boolean
      is
         L       : GPR2.Build.Actions.Link.Object;

      begin
         L.Initialize_Library (View);

         if not Tree_Db.Add_Action (L) then
            return False;
         end if;

         if not Tree_Db.Add_Output (L.UID, L.Output) then
            return False;
         end if;

         --  ??? TODO: Take care of Library_Interface

         if not View.Is_Externally_Built then
            for CU of View.Own_Units loop
               declare
                  Comp : GPR2.Build.Actions.Compile.Ada.Object;
               begin
                  Comp.Initialize (CU);
                  if not Tree_Db.Add_Action (Comp) then
                     return False;
                  end if;

                  Tree_Db.Add_Input
                    (L.UID, Comp.Object_File, False);
               end;
            end loop;

            for Src of View.Sources loop
               if not Src.Has_Units
                 and then Src.Is_Compilable
                 and then Src.Kind = S_Body
               then
                  declare
                     Comp : GPR2.Build.Actions.Compile.Object;
                  begin
                     Comp.Initialize (Src);

                     if not Tree_Db.Has_Action (Comp.UID) then
                        if not Tree_Db.Add_Action (Comp) then
                           return False;
                        end if;
                     end if;

                     Tree_Db.Add_Input
                       (L.UID, Comp.Object_File, False);
                  end;
               end if;
            end loop;
         end if;

         return Tree_Db.Propagate_Actions;
      end Add_Actions_To_Build_Lib;

      -------------------------------
      -- Add_Actions_To_Build_Main --
      -------------------------------

      function Add_Actions_To_Build_Main
        (Main : GPR2.Build.Compilation_Unit.Unit_Location) return Boolean
      is
         Comp   : GBA.Compile.Ada.Object;
         Bind   : GBA.Ada_Bind.Object;
         Link   : GBA.Link.Object;
         Source : constant GPR2.Build.Source.Object :=
                    Main.View.Source (Main.Source.Simple_Name);

      begin
         Link.Initialize_Executable
           (GPR2.Build.Artifacts.File_Part.Create (Main.Source, Main.Index),
            Main.View);
         if Tree_Db.Has_Action (Link.UID) then
            return True;
         end if;

         if not Tree_Db.Add_Action (Link) then
            return False;
         end if;

         Comp.Initialize
           (Main.View.Unit
              (Source.Units.Element (Main.Index).Name));

         if not Tree.Artifacts_Database.Has_Action (Comp.UID) then
            if not Tree_Db.Add_Action (Comp) then
               return False;
            end if;
         end if;

         Bind.Initialize (Comp.Ali_File, Main.View);
         if not Tree_Db.Add_Action (Bind) then
            return False;
         end if;


         Tree_Db.Add_Input (Bind.UID, Comp.Ali_File, True);
         Tree_Db.Add_Input (Link.UID, Comp.Object_File, False);
         Tree_Db.Add_Input (Link.UID, Bind.Post_Bind.Object_File, False);

         for Src of Main.View.Sources loop
            if not Src.Has_Units
              and then Src.Is_Compilable
              and then Src.Kind = S_Body
            then
               declare
                  Comp : GPR2.Build.Actions.Compile.Object;
               begin
                  Comp.Initialize (Src);

                  if not Tree_Db.Has_Action (Comp.UID) then
                     if not Tree_Db.Add_Action (Comp) then
                        return False;
                     end if;
                  end if;

                  Tree_Db.Add_Input
                    (Link.UID, Comp.Object_File, False);
               end;
            end if;
         end loop;

         --  Add the libraries present in the closure as dependencies

         for V of Main.View.Closure loop
            if not V.Is_Extended and then V.Is_Library then
               declare
                  Lib_Id : constant GBA.Link.Link_Id :=
                             GBA.Link.Create
                               (V, V.Library_Filename.Simple_Name, True);
                  Lib_A  : constant GBA.Link.Object'Class :=
                             GBA.Link.Object'Class (Tree_Db.Action (Lib_Id));
                  use GPR2.Build.Actions;
               begin
                  --  Ensure the lib and bind phases don't occur before
                  --  depending libraries are complete
                  Tree_Db.Add_Input (Link.UID, Lib_A.Output, False);
                  Tree_Db.Add_Input (Bind.UID, Lib_A.Output, False);
               end;
            end if;
         end loop;

         return True;
      end Add_Actions_To_Build_Main;

   begin
      --  First create actions for the libraries in the closure
      for V of Tree.Ordered_Views loop
         if not V.Is_Extended
           and then V.Is_Library
         then
            if not Add_Actions_To_Build_Lib (V) then
               return False;
            end if;
         end if;
      end loop;

      --  Add inter-libraries dependencies
      for Lib of Tree.Ordered_Views loop
         if not Lib.Is_Extended
           and then Lib.Is_Library
         then
            declare
               Lib_Id : constant GBA.Link.Link_Id :=
                          GBA.Link.Create
                            (Lib, Lib.Library_Filename.Simple_Name, True);
            begin
               for V of Lib.Closure (False) loop
                  if not V.Is_Extended
                    and then V.Is_Library
                  then
                     declare
                        Dep_Id : constant GBA.Link.Link_Id :=
                                   GBA.Link.Create
                                     (V, V.Library_Filename.Simple_Name, True);
                        Dep_A  : constant GBA.Link.Object'Class :=
                                   GBA.Link.Object'Class
                                     (Tree_Db.Action (Dep_Id));
                     begin
                        Tree_Db.Add_Input (Lib_Id, Dep_A.Output, False);
                     end;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      for Root of Tree.Namespace_Root_Projects loop
         if Root.Has_Mains then
            for Main of Root.Mains loop
               if not Add_Actions_To_Build_Main (Main) then
                  return False;
               end if;
            end loop;

            --  ??? TODO handle non Ada sources

            if not Tree.Artifacts_Database.Propagate_Actions then
               return False;
            end if;
         elsif not Root.Is_Library then
            for Source of Root.Sources loop
               if Source.Is_Compilable then
                  if not Source.Has_Units then
                     if Source.Kind = S_Body then
                        declare
                           Comp : GPR2.Build.Actions.Compile.Object;
                        begin
                           Comp.Initialize (Source);
                           if not Tree.Artifacts_Database.Add_Action (Comp)
                           then
                              return False;
                           end if;
                        end;
                     end if;

                  elsif Source.Language = Ada_Language then
                     for Unit of Source.Units loop
                        if Unit.Kind = S_Body then
                           declare
                              Comp : GPR2.Build.Actions.Compile.Ada.Object;
                           begin
                              Comp.Initialize
                                (Root.Unit (Unit.Name));
                              if not Tree.Artifacts_Database.Add_Action (Comp)
                              then
                                 return False;
                              end if;
                           end;
                        end if;
                     end loop;
                  else
                     raise Program_Error with
                       "unexpected source with units not being Ada";
                  end if;
               end if;
            end loop;
         end if;
      end loop;

      return True;
   end Add_Actions_To_Build_Mains;
end GPRtools.Actions;
