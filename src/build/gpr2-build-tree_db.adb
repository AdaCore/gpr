--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.View_Tables;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.View_Ids.Set;

package body GPR2.Build.Tree_Db is

   package PRA renames GPR2.Project.Registry.Attribute;

   use type GPR2.View_Ids.View_Id;

   procedure Check_Tree (Self : in out Object)
   is
      To_Remove : GPR2.View_Ids.Set.Set;
   begin
      --  Check for new views

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind
           and then not Self.Build_Dbs.Contains (V.Id)
         then
            declare
               Db_Data : View_Tables.View_Data
                           (Is_Root => V.Is_Namespace_Root);
               Db_Inst : View_Db.Object;
            begin
               Db_Data.View    := V;
               Db_Data.Tree_Db := Self.Self;
               Db_Inst := View_Tables.View_Base_For (Db_Data);
               Self.Build_Dbs.Insert (V.Id, Db_Inst);
               --  Db_Inst.Update;
            end;
         end if;
      end loop;

      --  Check for deleted views

      for C in Self.Build_Dbs.Iterate loop
         declare
            Id : constant View_Ids.View_Id := Build_DB_Maps.Key (C);
         begin
            if not Self.Tree.Get_View (Id).Is_Defined then
               To_Remove.Include (Id);
            end if;
         end;
      end loop;

      for Id of To_Remove loop
         Self.Build_Dbs.Delete (Id);
      end loop;
   end Check_Tree;

   ----------
   -- Load --
   ----------

   procedure Create
     (Self                 : in out Object;
      Tree                 : GPR2.Project.Tree.Object;
      With_Runtime_Sources : Boolean)
   is
      Db_Inst : View_Db.Object;

   begin

      Self.Self := Self'Unrestricted_Access;
      Self.Tree := Tree.Reference;
      Self.With_RTS := With_Runtime_Sources;

      --  Source files are propagated from the source owner (e.g. the view that
      --  defines the source directory where we found the source) to
      --  the other views (aggregate libraries or extending projects).
      --
      --  So for this to work efficiently, we need to use a topological order
      --  to populate the sources.

      for V of Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind then
            declare
               Db_Data : View_Tables.View_Data
                           (Is_Root => V.Is_Namespace_Root);
            begin
               Db_Data.View    := V;
               Db_Data.Tree_Db := Self.Self;
               Db_Inst := View_Tables.View_Base_For (Db_Data);
               Self.Build_Dbs.Insert (V.Id, Db_Inst);
               --  Db_Inst.Update;
            end;
         end if;
      end loop;
   end Create;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self     : in out Object;
      Option   : Source_Info_Option;
      Messages : out GPR2.Log.Object)
   is
   begin
      Self.Src_Option := Option;

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind then
            View_Tables.Check_Source_Lists
              (View_Tables.Get_Data (Self.Self, V), Messages);
         end if;
      end loop;

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Object_Dir_Kind
           and then (Self.With_RTS or else V.Id /= View_Ids.Runtime_View_Id)
         then
            View_Tables.Refresh (View_Tables.Get_Data (Self.Self, V),
                                 Messages);
         end if;
      end loop;

      for V of Self.Tree.Ordered_Views loop
         if V.Kind in With_Source_Dirs_Kind
           and then V.Id /= View_Ids.Runtime_View_Id
         then
            declare
               SF : constant Project.Attribute.Object :=
                      V.Attribute (PRA.Source_Files);
               V_Db : constant GPR2.Build.View_Tables.View_Data_Ref :=
                        View_Tables.Get_Data (Self.Self, V);
            begin
               if not SF.Is_Defined
                 or else not SF.Values.Is_Empty
               then
                  for L of V.Languages loop
                     declare
                        Lang : constant Language_Id := +Name_Type (L.Text);
                     begin
                        if not V_Db.Langs_Usage.Contains (Lang)
                          or else V_Db.Langs_Usage (Lang) = 0
                        then
                           Messages.Append
                             (Message.Create
                                (Message.Warning,
                                 "there are no sources of language """ & L.Text
                                 & """ in this project",
                                 L));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end if;
      end loop;

      for V of Self.Tree.Namespace_Root_Projects loop
         V.Check_Mains (Messages);
      end loop;

      if Self.Src_Option >= Sources_Units then
         for V of Self.Tree.Namespace_Root_Projects loop
            if V.Kind in With_Object_Dir_Kind then
               declare
                  V_Db : constant View_Tables.View_Data_Ref :=
                           View_Tables.Get_Data (Self.Self, V);
               begin
                  for U of V_Db.CUs loop
                     U.Check_Name_Validity (Messages);
                  end loop;
               end;
            end if;
         end loop;

         for V of Self.Tree.Ordered_Views loop
            if V.Kind in With_Object_Dir_Kind then
               declare
                  use GPR2.Containers;
                  V_Db            : constant View_Tables.View_Data_Ref :=
                                      View_Tables.Get_Data (Self.Self, V);
               begin
                  for C in V.Interface_Units.Iterate loop
                     if not V_Db.Own_CUs.Contains
                       (Unit_Name_To_Sloc.Key (C))
                     then
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "source for interface unit '" &
                                String (Unit_Name_To_Sloc.Key (C)) &
                                "' not found",
                              Unit_Name_To_Sloc.Element (C)));
                     end if;
                  end loop;

                  for C in V.Interface_Sources.Iterate loop
                     if not V_Db.Sources.Contains
                       (Source_Path_To_Sloc.Key (C))
                     then
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "source for interface '" &
                                String (Source_Path_To_Sloc.Key (C)) &
                                "' not found",
                              Source_Path_To_Sloc.Element (C)));
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end if;
   end Refresh;

   function Tree (Self : Object) return access GPR2.Project.Tree.Object is
     (Self.Tree);

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Object)
   is
   begin
      Self.Build_Dbs.Clear;
      Self.Tree := null;
      Self.Self := null;
      Self.Src_Option := No_Source;
      Self.With_RTS   := False;
   end Unload;

   -------------------
   -- View_Database --
   -------------------

   function View_Database
     (Self : Object; View : GPR2.Project.View.Object)
      return Build.View_Db.Object
   is
   begin
      return Self.Build_Dbs.Element (View.Id);
   end View_Database;

end GPR2.Build.Tree_Db;
