------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;  use Ada.Text_IO;
with GNAT.Strings; use GNAT.Strings;

with GPR2.Path_Name;    use GPR2;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.Unit_Info;
with GPR2.Unit;

package body GPRls.Gnatdist is

   procedure Write_Char (Char : Character) renames Ada.Text_IO.Put;
   procedure Write_Str (Str : String) renames Ada.Text_IO.Put;
   procedure Write_Eol (Spacing : Positive_Count := 1)
                        renames Ada.Text_IO.New_Line;

   N_Flags   : Natural;
   N_Indents : Natural := 0;

   type Token_Type is
     (T_No_ALI,
      T_ALI,
      T_Unit,
      T_With,
      T_Source,
      T_Afile,
      T_Ofile,
      T_Sfile,
      T_Name,
      T_Main,
      T_Kind,
      T_Flags,
      T_Preelaborated,
      T_Pure,
      T_Has_RACW,
      T_Remote_Types,
      T_Shared_Passive,
      T_RCI,
      T_Predefined,
      T_Internal,
      T_Is_Generic,
      T_Procedure,
      T_Function,
      T_Package,
      T_Subprogram,
      T_Spec,
      T_Body);

   Image : constant array (Token_Type) of String_Access :=
             (T_No_ALI         => new String'("No_ALI"),
              T_ALI            => new String'("ALI"),
              T_Unit           => new String'("Unit"),
              T_With           => new String'("With"),
              T_Source         => new String'("Source"),
              T_Afile          => new String'("Afile"),
              T_Ofile          => new String'("Ofile"),
              T_Sfile          => new String'("Sfile"),
              T_Name           => new String'("Name"),
              T_Main           => new String'("Main"),
              T_Kind           => new String'("Kind"),
              T_Flags          => new String'("Flags"),
              T_Preelaborated  => new String'("Preelaborated"),
              T_Pure           => new String'("Pure"),
              T_Has_RACW       => new String'("Has_RACW"),
              T_Remote_Types   => new String'("Remote_Types"),
              T_Shared_Passive => new String'("Shared_Passive"),
              T_RCI            => new String'("RCI"),
              T_Predefined     => new String'("Predefined"),
              T_Internal       => new String'("Internal"),
              T_Is_Generic     => new String'("Is_Generic"),
              T_Procedure      => new String'("procedure"),
              T_Function       => new String'("function"),
              T_Package        => new String'("package"),
              T_Subprogram     => new String'("subprogram"),
              T_Spec           => new String'("spec"),
              T_Body           => new String'("body"));

   procedure Output_Name  (N : Name_Type);
   --  Remove any encoding info (%b and %s) and output N

   procedure Output_Afile (A : Simple_Name);
   procedure Output_Ofile
     (Source : GPR2.Project.Source.Object; Index : Unit_Index);
   procedure Output_Sfile (S   : Path_Name.Object);
   --  Output various names. Check that the name is different from no name.
   --  Otherwise, skip the output.

   procedure Output_Token (T : Token_Type);
   --  Output token using specific format. That is several indentations and:
   --
   --  T_No_ALI  .. T_With : <token> & " =>" & NL
   --  T_Source  .. T_Kind : <token> & " => "
   --  T_Flags             : <token> & " =>"
   --  T_Preelab .. T_Body : " " & <token>

   procedure Output_Sdep (S : Simple_Name);
   procedure Output_Unit (Unit : GPR2.Unit.Object; S : Project.Source.Object);
   procedure Output_With (Tree : Project.Tree.Object; W : Name_Type);
   --  Output this entry as a global section (like ALIs)

   ------------------
   -- Output_Afile --
   ------------------

   procedure Output_Afile (A : Simple_Name) is
   begin
      Output_Token (T_Afile);
      Write_Str (String (A));
      Write_Eol;
   end Output_Afile;

   ----------------
   -- Output_ALI --
   ----------------

   procedure Output_ALI
     (Source : GPR2.Project.Source.Object; Index : GPR2.Unit_Index)
   is
      use type GPR2.Unit.Main_Type;
      Unit : constant GPR2.Unit.Object :=
               Source.Unit (Index);
      Part : GPR2.Unit.Object;
      Main : constant GPR2.Unit.Main_Type := Unit.Main_Kind;

      procedure Print_Dependency (Source    : GPR2.Project.Source.Object;
                                  Unit      : GPR2.Unit.Object;
                                  Timestamp : Ada.Calendar.Time);

      ----------------------
      -- Print_Dependency --
      ----------------------

      procedure Print_Dependency (Source    : GPR2.Project.Source.Object;
                                  Unit      : GPR2.Unit.Object;
                                  Timestamp : Ada.Calendar.Time)
      is
         pragma Unreferenced (Unit, Timestamp);
      begin
         Output_Sdep (Source.Path_Name.Simple_Name);
      end Print_Dependency;

   begin
      Output_Token (T_ALI);
      N_Indents := N_Indents + 1;

      Output_Afile (Source.Artifacts.Dependency (Index).Simple_Name);
      Output_Ofile (Source, Index);
      Output_Sfile (Source.Path_Name);

      --  Output Main

      if Main /= GPR2.Unit.None then
         Output_Token (T_Main);
         Output_Token
           (if Main = GPR2.Unit.Is_Procedure then T_Procedure else T_Function);
         Write_Eol;
      end if;

      --  Output Units

      Output_Unit (Unit, Source);
      declare
         Other : constant GPR2.Project.Source.Object :=
                   Source.Other_Part_Unchecked (Index).Source;
      begin
         if Other.Is_Defined
           and then Other.Check_Unit
            (Unit.Name, Unit.Kind not in GPR2.Unit.Spec_Kind, Part)
         then
            Output_Unit (Part, Other);
         end if;
      end;

      --  Output Sdeps

      Source.Dependencies
        (Index, Print_Dependency'Access);

      N_Indents := N_Indents - 1;
   end Output_ALI;

   -----------------
   -- Output_Name --
   -----------------

   procedure Output_Name (N : Name_Type) is
   begin
      Output_Token (T_Name);

      --  po_gnatdist need unit name lowercased

      Write_Str (To_Lower (N));
      Write_Eol;
   end Output_Name;

   -------------------
   -- Output_No_ALI --
   -------------------

   procedure Output_No_ALI
     (Source : GPR2.Project.Source.Object; Index : GPR2.Unit_Index) is
   begin
      Output_Token (T_No_ALI);
      N_Indents := N_Indents + 1;
      Output_Afile (Source.Artifacts.Dependency (Index).Simple_Name);
      N_Indents := N_Indents - 1;
   end Output_No_ALI;

   ------------------
   -- Output_Ofile --
   ------------------

   procedure Output_Ofile
     (Source : GPR2.Project.Source.Object; Index : Unit_Index) is
   begin
      Output_Token (T_Ofile);
      Write_Str (Source.Artifacts.Object_Code (Index).Value);
      Write_Eol;
   end Output_Ofile;

   -----------------
   -- Output_Sdep --
   -----------------

   procedure Output_Sdep (S : Simple_Name) is
   begin
      Output_Token (T_Source);
      Write_Str (String (S));
      Write_Eol;
   end Output_Sdep;

   ------------------
   -- Output_Sfile --
   ------------------

   procedure Output_Sfile (S : Path_Name.Object) is
   begin
      Output_Token (T_Sfile);
      Write_Str (S.Value);
      Write_Eol;
   end Output_Sfile;

   ------------------
   -- Output_Token --
   ------------------

   procedure Output_Token (T : Token_Type) is
   begin
      if T in T_No_ALI .. T_Flags then
         for J in 1 .. N_Indents loop
            Write_Str ("   ");
         end loop;

         Write_Str (Image (T).all);

         for J in Image (T)'Length .. 12 loop
            Write_Char (' ');
         end loop;

         Write_Str ("=>");

         if T in T_No_ALI .. T_With then
            Write_Eol;
         elsif T in T_Source .. T_Name then
            Write_Char (' ');
         end if;

      elsif T in T_Preelaborated .. T_Body then
         if T in T_Preelaborated .. T_Is_Generic then
            if N_Flags = 0 then
               Output_Token (T_Flags);
            end if;

            N_Flags := N_Flags + 1;
         end if;

         Write_Char (' ');
         Write_Str  (Image (T).all);

      else
         Write_Str  (Image (T).all);
      end if;
   end Output_Token;

   -----------------
   -- Output_Unit --
   -----------------

   procedure Output_Unit
     (Unit : GPR2.Unit.Object; S : Project.Source.Object)
   is
      Tree : constant access Project.Tree.Object := S.View.Tree;

      procedure Check_Flag (Flag : GPR2.Unit.Flag; Token : Token_Type);

      ----------------
      -- Check_Flag --
      ----------------

      procedure Check_Flag (Flag : GPR2.Unit.Flag; Token : Token_Type) is
      begin
         if Unit.Is_Flag_Set (Flag) then
            Output_Token (Token);
         end if;
      end Check_Flag;

   begin
      Output_Token (T_Unit);
      N_Indents := N_Indents + 1;

      --  Output Name

      Output_Name (Unit.Name);

      --  Output Kind

      Output_Token (T_Kind);

      Output_Token
        (case Unit.Library_Item_Kind is
            when GPR2.Unit.Is_Package    => T_Package,
            when GPR2.Unit.Is_Subprogram => T_Subprogram);

      Output_Token
        (if Unit.Kind in GPR2.Unit.Spec_Kind then T_Spec else T_Body);
      Write_Eol;

      Output_Sfile (S.Path_Name);

      --  Output Flags

      N_Flags := 0;

      Check_Flag (GPR2.Unit.Preelab,        T_Preelaborated);
      Check_Flag (GPR2.Unit.Pure,           T_Pure);
      Check_Flag (GPR2.Unit.Has_RACW,       T_Has_RACW);
      Check_Flag (GPR2.Unit.Remote_Types,   T_Remote_Types);
      Check_Flag (GPR2.Unit.Shared_Passive, T_Shared_Passive);
      Check_Flag (GPR2.Unit.RCI,            T_RCI);
      Check_Flag (GPR2.Unit.Predefined,     T_Predefined);
      Check_Flag (GPR2.Unit.Is_Generic,     T_Is_Generic);

      if N_Flags > 0 then
         Write_Eol;
      end if;

      --  Output Withs

      for W of S.Context_Clause_Dependencies (Unit.Index)
      loop
         Output_With (Tree.all, W.Text);
      end loop;

      N_Indents := N_Indents - 1;
   end Output_Unit;

   -----------------
   -- Output_With --
   -----------------

   procedure Output_With (Tree : Project.Tree.Object; W : Name_Type) is
      View  : constant Project.View.Object := Tree.Get_View (Unit => W);
      pragma Assert (View.Is_Defined, "unit undefined: " & String (W));
      UI    : constant Project.Unit_Info.Object := View.Unit (W);
      Src   : constant Project.Source.Object :=
                View.Source ((if UI.Has_Body
                             then UI.Main_Body.Source
                             else UI.Spec.Source));
      Index : constant GPR2.Unit_Index :=
                (if UI.Has_Body then UI.Main_Body.Index else UI.Spec.Index);
      Afile : Path_Name.Object;

   begin
      pragma Assert
        (Src.Is_Defined,
         "source undefined: View " &
           String (View.Name) & " Unit " & String (W));

      Afile := Project.Source.Artifact.Dependency (Src, Index);

      Output_Token (T_With);
      N_Indents := N_Indents + 1;

      Output_Name (W);

      --  Output Kind

      Output_Token (T_Kind);

      Output_Token (if UI.Has_Spec then T_Spec else T_Body);

      Write_Eol;

      if Afile.Is_Defined then
         Output_Afile (Afile.Simple_Name);
      end if;

      Output_Sfile (Src.Path_Name);

      N_Indents := N_Indents - 1;
   end Output_With;

end GPRls.Gnatdist;
