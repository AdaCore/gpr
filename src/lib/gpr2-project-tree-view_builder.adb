------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GPR2.Project.Attribute;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Variable.Set;
with GPR2.Project.Pack;
with GPR2.Project.Parser.Create;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Pack;

package body GPR2.Project.Tree.View_Builder is

   procedure Set_Attribute
     (Self   : in out Object;
      Q_Name : Registry.Attribute.Qualified_Name;
      Attr   : GPR2.Project.Attribute.Object);

   function SR_Attr
     (Self : Object;
      Attr : Registry.Attribute.Qualified_Name)
      return Source_Reference.Attribute.Object
   is (GPR2.Source_Reference.Attribute.Object
       (GPR2.Source_Reference.Attribute.Create
          (Self.Data.Trees.Project.Path_Name.Value, 0, 0,
           Attr.Attr)));

   function SR_Index
     (Self  : Object;
      Index : Value_Type)
     return GPR2.Project.Attribute_Index.Object
   is (GPR2.Project.Attribute_Index.Create
         (GPR2.Source_Reference.Value.Object
            (GPR2.Source_Reference.Value.Create
               (Self.Data.Trees.Project.Path_Name.Value, 0, 0,
                Index)),
          Is_Others      => False,
          Case_Sensitive => False));

   function SR_Value
     (Self : Object; Value : Value_Type) return Source_Reference.Value.Object
   is (GPR2.Source_Reference.Value.Object
       (GPR2.Source_Reference.Value.Create
          (Self.Data.Trees.Project.Path_Name.Value, 0, 0,
           Text     => Value)));

   function SR_Values
     (Self : Object; Values : Containers.Value_List)
      return Containers.Source_Value_List;

   ------------
   -- Create --
   ------------

   function Create
     (Project_Dir : GPR2.Path_Name.Object;
      Name        : Name_Type;
      Qualifier   : Project_Kind := K_Standard) return Object
   is
      Gpr    : constant Filename_Type :=
                 Filename_Type (To_Lower (Name)) & ".gpr";
      Gpr_Path : constant GPR2.Path_Name.Object :=
                   Project_Dir.Compose (Gpr);
      Result : Object;

   begin
      Result.Data.Kind          := Qualifier;
      Result.Data.Path          := Project_Dir;
      Result.Data.Is_Root       := True;
      Result.Data.Unique_Id     := GPR2.View_Ids.Create (Gpr_Path);
      Result.Data.Trees.Project := GPR2.Project.Parser.Create
        (Name      => Name,
         File      => Gpr_Path,
         Qualifier => Qualifier);

      return Result;
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self             : in out Tree.Object;
      Project          : Object;
      Context          : GPR2.Context.Object;
      Config           : PC.Object                 := PC.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Boolean                   := False;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference)
   is
   begin
      Self.Load ((Project_Definition, Project.Data),
                 Context          => Context,
                 Config           => Config,
                 Build_Path       => Build_Path,
                 Subdirs          => Subdirs,
                 Src_Subdirs      => Src_Subdirs,
                 Check_Shared_Lib => Check_Shared_Lib,
                 Absent_Dir_Error => Absent_Dir_Error,
                 Implicit_With    => Implicit_With,
                 Pre_Conf_Mode    => Pre_Conf_Mode,
                 File_Reader      => File_Reader);
   end Load;

   procedure Load_Autoconf
     (Self              : in out Tree.Object;
      Project           : Object;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object        := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type      := No_Name;
      Src_Subdirs       : Optional_Name_Type      := No_Name;
      Check_Shared_Lib  : Boolean                 := True;
      Absent_Dir_Error  : Boolean                 := False;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Target            : Optional_Name_Type      := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
      Config_Project    : GPR2.Path_Name.Object   :=
                            GPR2.Path_Name.Undefined;
      File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                            GPR2.File_Readers.No_File_Reader_Reference)
   is
   begin
      Self.Load_Autoconf
        (Root_Project      => (Project_Definition, Project.Data),
         Context           => Context,
         Build_Path        => Build_Path,
         Subdirs           => Subdirs,
         Src_Subdirs       => Src_Subdirs,
         Check_Shared_Lib  => Check_Shared_Lib,
         Absent_Dir_Error  => Absent_Dir_Error,
         Implicit_With     => Implicit_With,
         Target            => Target,
         Language_Runtimes => Language_Runtimes,
         Base              => Base,
         Config_Project    => Config_Project,
         File_Reader       => File_Reader);
   end Load_Autoconf;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self   : in out Object;
      Q_Name : Registry.Attribute.Qualified_Name;
      Attr   : GPR2.Project.Attribute.Object)
   is
   begin
      if Q_Name.Pack = No_Package then
         Self.Data.Attrs.Include (Attr);

      elsif not Self.Data.Packs.Contains (Q_Name.Pack) then
         declare
            Pack : GPR2.Project.Pack.Object;
         begin
            Pack := GPR2.Project.Pack.Object'
              (Source_Reference.Pack.Object
                 (Source_Reference.Pack.Create
                      (Attr.Filename, 0, 0, Q_Name.Pack)) with
               Project.Attribute.Set.Empty_Set,
               Project.Variable.Set.Empty_Set);
            Pack.Attrs.Insert (Attr);
            Self.Data.Packs.Insert (Q_Name.Pack, Pack);
         end;

      else
         Self.Data.Packs (Q_Name.Pack).Attrs.Include (Attr);
      end if;
   end Set_Attribute;

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Registry.Attribute.Qualified_Name;
      Value : Value_Type)
   is
   begin
      Self.Set_Attribute
        (Attr,
         GPR2.Project.Attribute.Create
           (SR_Attr (Self, Attr), SR_Value (Self, Value)));
   end Set_Attribute;

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Registry.Attribute.Qualified_Name;
      Values : Containers.Value_List)
   is
   begin
      Self.Set_Attribute
        (Attr,
         Project.Attribute.Create
           (SR_Attr (Self, Attr),
            SR_Values (Self, Values)));
   end Set_Attribute;

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Registry.Attribute.Qualified_Name;
      Index : Value_Type;
      Value : Value_Type)
   is
   begin
      Self.Set_Attribute
        (Attr,
         GPR2.Project.Attribute.Create
           (SR_Attr (Self, Attr),
            SR_Index (Self, Index),
            SR_Value (Self, Value)));
   end Set_Attribute;

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Registry.Attribute.Qualified_Name;
      Index  : Value_Type;
      Values : Containers.Value_List)
   is
   begin
      Self.Set_Attribute
        (Attr,
         GPR2.Project.Attribute.Create
           (SR_Attr (Self, Attr),
            SR_Index (Self, Index),
            SR_Values (Self, Values)));
   end Set_Attribute;

   ---------------
   -- SR_Values --
   ---------------

   function SR_Values
     (Self : Object; Values : Containers.Value_List)
      return Containers.Source_Value_List
   is
      Result : Containers.Source_Value_List;
   begin
      for V of Values loop
         Result.Append (SR_Value (Self, V));
      end loop;

      return Result;
   end SR_Values;

end GPR2.Project.Tree.View_Builder;
