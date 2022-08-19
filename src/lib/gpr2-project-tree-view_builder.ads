--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package GPR2.Project.Tree.View_Builder is

   type Object is tagged private;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Project_Dir : GPR2.Path_Name.Object;
      Name        : Name_Type;
      Qualifier   : Project_Kind := K_Standard) return Object;

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Value : Value_Type);

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Values : Containers.Value_List);

   procedure Set_Attribute
     (Self  : in out Object;
      Attr  : Q_Attribute_Id;
      Index : Value_Type;
      Value : Value_Type);

   procedure Set_Attribute
     (Self   : in out Object;
      Attr   : Q_Attribute_Id;
      Index  : Value_Type;
      Values : Containers.Value_List);

   procedure Load_Autoconf
     (Self              : in out Tree.Object;
      Project           : View_Builder.Object;
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
       with Pre => Project.Is_Defined;

   procedure Load
     (Self             : in out Tree.Object;
      Project          : View_Builder.Object;
      Context          : GPR2.Context.Object;
      Config           : GPR2.Project.Configuration.Object :=
                           GPR2.Project.Configuration.Undefined;
      Build_Path       : Path_Name.Object        := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type      := No_Name;
      Src_Subdirs      : Optional_Name_Type      := No_Name;
      Check_Shared_Lib : Boolean                 := True;
      Absent_Dir_Error : Boolean                 := False;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference)
       with Pre => Project.Is_Defined;
   --  Loads a root project. Similar to Tree.Load, using a View_Builder
   --  object as root project.

private

   type Object is tagged record
      Data : GPR2.Project.Definition.Data;
   end record;

   function Is_Defined (Self : Object) return Boolean is
     (Self.Data.Path.Is_Defined);

end GPR2.Project.Tree.View_Builder;
