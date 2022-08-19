--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Path_Name;
with GPR2.View_Ids;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Part_Set;
with GPR2.Source_Info;

package body GPR2.C.Utils is

   function To_Dir (Path : String) return GPR2.Path_Name.Object;

   -----------------
   -- Add_Path --
   -----------------

   procedure Add_Path (Paths : in out GPR_Paths; Path : String) is
   begin
      Paths.Append (GPR2.Path_Name.Create_File (Filename_Optional (Path)));
   end Add_Path;

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (Tree : GPR_Tree) return String is
   begin
      return String (Tree.Archive_Suffix);
   end Archive_Suffix;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (View  : GPR_View; Name : String; Pkg : String := Empty_String;
      Index : Index_Type := No_Index) return GPR_Attribute
   is
   begin
      return
        View.Attribute
          (Name  => (GPR2."+" (Optional_Name_Type (Pkg)),
                     GPR2."+" (Optional_Name_Type (Name))),
           Index => Index.GPR2_Index, At_Pos => Index.Position);
   end Attribute;

   --------------------
   -- Callgraph_File --
   --------------------

   function Callgraph_File (Source : GPR_Source) return String
is
      Artifact : constant GPR2.Project.Source.Artifact.Object :=
        GPR2.Project.Source.Artifact.Create (Source => Source);
   begin
      if Artifact.Has_Callgraph then
         return String (Artifact.Callgraph.Value);
      else
         return Empty_String;
      end if;
   end Callgraph_File;

   -------------------
   -- Coverage_File --
   -------------------

   function Coverage_File (Source : GPR_Source) return String
is
      Artifact : constant GPR2.Project.Source.Artifact.Object :=
        GPR2.Project.Source.Artifact.Create (Source => Source);
   begin
      if Artifact.Has_Coverage then
         return String (Artifact.Coverage.Value);
      else
         return Empty_String;
      end if;
   end Coverage_File;

   ----------------
   -- Delete_Var --
   ----------------

   procedure Delete_External_Variable (Ctx : in out GPR_Context; Name : String)
   is
   begin
      Ctx.Exclude (Optional_Name_Type (Name));
   end Delete_External_Variable;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Source : GPR_Source; Closure : Boolean := False) return GPR_Sources
   is
      Parts : constant GPR2.Project.Source.Part_Set.Object :=
        Source.Dependencies (Closure => Closure);
      Result : GPR_Sources;
   begin
      for Part of Parts loop
         Result.Include (Part.Source);
      end loop;
      return Result;
   end Dependencies;

   ----------------
   -- Dependency --
   ----------------

   function Dependency_File
     (Source : GPR_Source; Index : Unit_Index := No_Unit_Index) return String
   is
      Artifact : constant GPR2.Project.Source.Artifact.Object :=
        GPR2.Project.Source.Artifact.Create (Source => Source);
   begin
      if Artifact.Has_Dependency (Index => Index) then
         return String (Artifact.Dependency (Index => Index).Value);
      else
         return Empty_String;
      end if;
   end Dependency_File;

   --------------
   -- Filename --
   --------------

   function Filename
     (Name : String; Position : Unit_Index := No_Unit_Index) return Index_Type
   is
   begin
      return
        (Kind       => Filename_Index,
         GPR2_Index =>
           GPR2.Project.Attribute_Index.Create
             (Name, Case_Sensitive => GPR2.File_Names_Case_Sensitive),
         Position => Position);
   end Filename;

   --------------
   -- Get_View --
   --------------

   function Get_View (Tree : GPR_Tree; Id : String) return GPR_View is
      GPR2_Id : constant GPR2.View_Ids.View_Id :=
        GPR2.View_Ids.Import (Name => GPR2.Value_Type (Id));
   begin
      return GPR2.Project.Tree.Instance_Of (Tree, GPR2_Id);
   end Get_View;

   --------------
   -- Language --
   --------------

   function Language (Name : String) return Index_Type is
   begin
      return
        (Kind       => Language_Index,
         GPR2_Index =>
           GPR2.Project.Attribute_Index.Create (Name, Case_Sensitive => False),
         Position => No_Unit_Index);
   end Language;

   function Library_Filename (View : GPR_View) return String is
   begin
      return String (View.Library_Filename.Value);
   end Library_Filename;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Tree             : in out GPR_Tree;
      Filename         : String;
      Context          : GPR_Context  := Empty_Context;
      Build_Path       : String       := Empty_String;
      Subdirs          : String       := Empty_String;
      Src_Subdirs      : String       := Empty_String;
      Project_Dir      : String       := Empty_String;
      Check_Shared_Lib : Boolean      := True;
      Absent_Dir_Error : Boolean      := False;
      Implicit_With    : GPR_Paths    := No_Paths;
      Config           : String       := Empty_String;
      Target           : String       := Empty_String;
      Runtimes         : GPR_Runtimes := No_Runtimes)
   is
      Project       : constant GPR2.Path_Name.Object :=
                        (if Filename'Length > 0
                         then GPR2.Path_Name.Create_File
                                (Filename_Type (Filename))
                         else To_Dir (Project_Dir));
      Build_Path_P  : constant GPR2.Path_Name.Object := To_Dir (Build_Path);

   begin
      if Config /= Empty_String then
         declare
            Config_Project : GPR2.Project.Configuration.Object;
         begin
            Config_Project :=
              GPR2.Project.Configuration.Load
                (GPR2.Path_Name.Create_File (Filename_Optional (Config)));
            Tree.Load
              (Filename         => Project,
               Context          => Context,
               Config           => Config_Project,
               Build_Path       => Build_Path_P,
               Subdirs          => Optional_Name_Type (Subdirs),
               Src_Subdirs      => Optional_Name_Type (Src_Subdirs),
               Check_Shared_Lib => Check_Shared_Lib,
               Absent_Dir_Error => Absent_Dir_Error,
               Implicit_With    => Implicit_With);
         end;
      else
         Tree.Load_Autoconf
           (Filename          => Project,
            Context           => Context,
            Build_Path        => Build_Path_P,
            Subdirs           => Optional_Name_Type (Subdirs),
            Src_Subdirs       => Optional_Name_Type (Src_Subdirs),
            Check_Shared_Lib  => Check_Shared_Lib,
            Absent_Dir_Error  => Absent_Dir_Error,
            Implicit_With     => Implicit_With,
            Target            => Optional_Name_Type (Target),
            Language_Runtimes => Runtimes);
      end if;
   end Load_Project;

   ----------
   -- Name --
   ----------

   function Name (Name : String) return Index_Type is
   begin
      return
        (Kind       => Filename_Index,
         GPR2_Index =>
           GPR2.Project.Attribute_Index.Create (Name, Case_Sensitive => False),
         Position => No_Unit_Index);
   end Name;

   function Object_File
     (Source : GPR_Source; Index : Unit_Index := No_Unit_Index) return String
   is
      Artifact : constant GPR2.Project.Source.Artifact.Object :=
        GPR2.Project.Source.Artifact.Create (Source => Source);
   begin
      if Artifact.Has_Object_Code (Index => Index) then
         return String (Artifact.Object_Code (Index => Index).Value);
      else
         return Empty_String;
      end if;
   end Object_File;

   procedure Prepend_Search_Path (Tree : in out GPR_Tree; Path : String) is
   begin
      Tree.Register_Project_Search_Path
        (Dir => GPR2.Path_Name.Create_Directory (Filename_Optional (Path)));
   end Prepend_Search_Path;

   ---------------
   -- Root_View --
   ---------------

   function Root_View (Tree : GPR_Tree) return GPR_View is
   begin
      return Tree.Root_Project;
   end Root_View;

   ------------------
   -- Runtime_View --
   ------------------

   function Runtime_View (Tree : GPR_Tree) return GPR_View is
   begin
      if Tree.Has_Runtime_Project then
         return Tree.Runtime_Project;
      else
         return Undefined_View;
      end if;
   end Runtime_View;

   ------------------
   -- Search_Paths --
   ------------------

   function Search_Paths (Tree : GPR_Tree) return GPR_Paths is
   begin
      return Tree.Project_Search_Paths;
   end Search_Paths;

   ---------------------------
   -- Set_External_Variable --
   ---------------------------

   procedure Set_External_Variable
     (Ctx : in out GPR_Context; Name : String; Value : String)
   is
   begin
      Ctx.Include (Optional_Name_Type (Name), Value);
   end Set_External_Variable;

   -----------------
   -- Set_Runtime --
   -----------------

   procedure Set_Runtime
     (Runtimes : in out GPR_Runtimes; Language : String; Runtime : String)
   is
   begin
      Runtimes.Include (GPR2."+" (Optional_Name_Type (Language)), Runtime);
   end Set_Runtime;

   ------------
   -- Source --
   ------------

   function Source (View : GPR_View; Path : String) return GPR_Source is
   begin
      return
        View.Source
          (File => GPR2.Path_Name.Create_File (Filename_Optional (Path)));
   end Source;

   ------------
   -- Target --
   ------------

   function Target (Tree : GPR_Tree) return String is
   begin
      return String (Tree.Target);
   end Target;

   ------------
   -- To_Dir --
   ------------

   function To_Dir (Path : String) return GPR2.Path_Name.Object is
   begin
      if Path = Empty_String then
         return GPR2.Path_Name.Undefined;
      else
         return GPR2.Path_Name.Create_Directory (Filename_Optional (Path));
      end if;
   end To_Dir;

   -------------------------
   -- Update_Source_Infos --
   -------------------------

   procedure Update_Source_Infos
     (Tree : GPR_Tree; Allow_Source_Parsing : Boolean := False)
   is
      use GPR2.Source_Info;
      Backend_List : Backend_Set := All_Backends;
   begin
      if not Allow_Source_Parsing then
         Backend_List (Source) := False;
      end if;

      Tree.Update_Sources
        (Stop_On_Error => False, With_Runtime => False,
         Backends      => Backend_List);
   end Update_Source_Infos;

   procedure Update_Source_Infos
     (Source : in out GPR_Source; Allow_Source_Parsing : Boolean := False)
   is
      use GPR2.Source_Info;
      Backend_List : Backend_Set := All_Backends;
   begin
      if not Allow_Source_Parsing then
         Backend_List (GPR2.Source_Info.Source) := False;
      end if;
      Source.Update (Backends => Backend_List);
   end Update_Source_Infos;

   ------------------------
   -- Update_Source_List --
   ------------------------

   procedure Update_Source_List (Tree : GPR_Tree)
   is
      use GPR2.Source_Info;
      Backend_List : constant Backend_Set := (LI => True, others => False);
   begin
      --  In theory we should call here Update_Sources with no backend to be
      --  as fast as possible. In practice calling Update_Sources with no
      --  backend, does not mark sources as loaded. Thus when looking for
      --  sources afterward in the view we reload everything with all backends
      --  including source backend (which is slow).
      --  Current workaround is to use the "LI" backend only.
      Tree.Update_Sources
        (Stop_On_Error => False,
         With_Runtime  => False,
         Backends      => Backend_List);
   end Update_Source_List;

end GPR2.C.Utils;
