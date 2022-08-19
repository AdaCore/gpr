------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GPR2.Version;

package body GPRtools.Command_Line is

   function "-" (S : String) return Unbounded_String renames
     To_Unbounded_String;
   function "-" (S : Switch_Type) return Unbounded_String is
      (To_Unbounded_String (String (S)));
   function "-" (S : Unbounded_String) return String renames
     To_String;
   function "-" (S : Unbounded_String) return Switch_Type is
      (Switch_Type (To_String (S)));

   procedure Add_Section_Argument_Internal
     (Self           : in out Command_Line_Parser;
      Name           : Switch_Type;
      Alt_Name       : String;
      Callback       : Section_Action;
      Help           : String := "";
      Index          : String := "";
      In_Switch_Attr : Boolean := True);

   function Get_Executable return String is
     (GNAT.Directory_Operations.Base_Name
        (Ada.Command_Line.Command_Name, ".exe"));

   function Get_RO
     (Self  : Command_Line_Parser;
      Group : Argument_Group) return Group_Maps.Constant_Reference_Type;

   function Get_RW
     (Self  : in out Command_Line_Parser;
      Group : Argument_Group) return Group_Maps.Reference_Type;

   procedure Base_Argument_Action
     (Parser : Command_Line_Parser'Class;
      Result : not null access Command_Line_Result'Class;
      Arg    : Switch_Type;
      Param  : String;
      Index  : String);

   type Parser_State is record
      Current_Section       : Unbounded_String;
      Current_Section_Index : Unbounded_String;
      Section_CB            : Section_Action;
   end record;

   procedure Get_Opt_Internal
     (Parser    : Command_Line_Parser;
      Pack      : GPR2.Package_Id;
      Arg       : String;
      Next      : String;
      Next_Used : out Boolean;
      State     : in out Parser_State;
      Result    : in out Command_Line_Result'Class);

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Self  : in out Command_Line_Parser;
      Group : Argument_Group;
      Def   : Argument_Definition)
   is
      Group_Ref : constant Group_Maps.Reference_Type := Self.Get_RW (Group);
      Temp      : Argument_Definition := Def;

   begin
      Temp.Group := Group;

      Self.Switches.Insert (-Def.Name, Temp);
      Group_Ref.Switches.Insert (-Def.Name);

      if Length (Def.Alt_Name) > 0 then
         Self.Aliases.Insert (-Def.Alt_Name, -Def.Name);
      end if;
   end Add_Argument;

   ------------------------
   -- Add_Argument_Group --
   ------------------------

   function Add_Argument_Group
     (Self     : in out Command_Line_Parser;
      Group    : Argument_Group;
      Name     : GPR2.Name_Type;
      Callback : Argument_Action;
      Help     : String := "";
      Last     : Boolean := False) return Argument_Group is
   begin
      Self.Groups.Insert
        (Name,
         Argument_Group_Internal'(Help      => -Help,
                                  Exclusive => False,
                                  Callback  => Callback,
                                  others    => <>));
      if Last then
         Self.Get_RW (Group).Last_Subgroups.Append (Name);
      else
         Self.Get_RW (Group).Subgroups.Append (Name);
      end if;

      return Argument_Group (-String (Name));
   end Add_Argument_Group;

   function Add_Argument_Group
     (Self     : in out Command_Line_Parser;
      Name     : GPR2.Name_Type;
      Callback : Argument_Action;
      Help     : String := "";
      Last     : Boolean := False) return Argument_Group is
   begin
      return Add_Argument_Group
               (Self, Self.Main_Group, Name, Callback, Help, Last);
   end Add_Argument_Group;

   -------------------------------------------
   -- Add_Mutually_Exclusive_Argument_Group --
   -------------------------------------------

   function Add_Mutually_Exclusive_Argument_Group
     (Self     : in out Command_Line_Parser;
      Group    : Argument_Group;
      Name     : GPR2.Name_Type;
      Help     : String := "";
      Required : Boolean := False) return Argument_Group
   is
      Ref : Group_Maps.Reference_Type renames Self.Get_RW (Group);
   begin
      Self.Groups.Insert
        (Name,
         Argument_Group_Internal'(Help      => -Help,
                                  Exclusive => True,
                                  Required  => Required,
                                  others    => <>));
      Ref.Subgroups.Append (Name);

      return Argument_Group (-String (Name));
   end Add_Mutually_Exclusive_Argument_Group;

   --------------------------
   -- Add_Section_Argument --
   --------------------------

   procedure Add_Section_Argument
     (Self           : in out Command_Line_Parser;
      Name           : Switch_Type;
      Callback       : Section_Action;
      Help           : String := "";
      Index          : String := "";
      In_Switch_Attr : Boolean := True) is
   begin
      Self.Add_Section_Argument_Internal
        (Name, "", Callback, Help, Index, In_Switch_Attr);
   end Add_Section_Argument;

   procedure Add_Section_Argument
     (Self           : in out Command_Line_Parser;
      Name           : Switch_Type;
      Alt_Name       : Switch_Type;
      Callback       : Section_Action;
      Help           : String := "";
      Index          : String := "";
      In_Switch_Attr : Boolean := True) is
   begin
      Self.Add_Section_Argument_Internal
        (Name, String (Alt_Name), Callback, Help, Index, In_Switch_Attr);
   end Add_Section_Argument;

   -----------------------------------
   -- Add_Section_Argument_Internal --
   -----------------------------------

   procedure Add_Section_Argument_Internal
     (Self           : in out Command_Line_Parser;
      Name           : Switch_Type;
      Alt_Name       : String;
      Callback       : Section_Action;
      Help           : String := "";
      Index          : String := "";
      In_Switch_Attr : Boolean := True)
   is
      Def : constant Argument_Definition :=
              Argument_Definition'
                (With_Value       => False,
                 Name             => To_Unbounded_String (Name),
                 Alt_Name         => To_Unbounded_String (Alt_Name),
                 Help             => To_Unbounded_String (Help),
                 Index            => To_Unbounded_String (Index),
                 In_Attr          => In_Switch_Attr,
                 Hidden           => False,
                 Group            => No_Group,
                 Is_Section       => True,
                 Section_Callback => Callback);
   begin
      Self.Add_Argument (To_Unbounded_String ("_root"), Def);

      --  Callback is null when the switch allows to go back to the default
      --  section.

      if Callback = null then
         if Length (Self.Default_Section) > 0 then
            raise Command_Line_Definition_Error with
              "Cannot define multiple section arguments: " &
              (-Self.Default_Section) & ", " & String (Name);
         end if;

         Self.Default_Section := -Name;
      end if;
   end Add_Section_Argument_Internal;

   --------------------------
   -- Base_Argument_Action --
   --------------------------

   procedure Base_Argument_Action
     (Parser : Command_Line_Parser'Class;
      Result : not null access Command_Line_Result'Class;
      Arg    : Switch_Type;
      Param  : String;
      Index  : String)
   is
      pragma Unreferenced (Result, Param, Index);
   begin
      if Arg = "-h" then
         Parser.Usage;
      elsif Arg = "--version" then
         Parser.Version;
      end if;

      GNAT.OS_Lib.OS_Exit_Default (0);
   end Base_Argument_Action;

   ------------
   -- Create --
   ------------

   function Create
     (Initial_Year : String;
      Cmd_Line     : String := "";
      Tool_Name    : String := "";
      Help         : String := "") return Command_Line_Parser'Class
   is
      Help_Arg    : constant Argument_Definition :=
                      Create ("-h",
                              Alt_Name       => "--help",
                              Help           => "Display usage and exit",
                              In_Switch_Attr => False);
      Version_Arg : constant Argument_Definition :=
                      Create ("--version",
                              "Display version and exit",
                              In_Switch_Attr => False);
      Parser      : Command_Line_Parser;

   begin
      Parser.Initial_Year  := -Initial_Year;
      Parser.Cmd_Line_Help := -Cmd_Line;
      Parser.Help          := -Help;

      if Tool_Name'Length > 0 then
         Parser.Tool := -Tool_Name;
      else
         Parser.Tool := -Get_Executable;
      end if;

      Parser.Groups.Insert
        ("_",
         Argument_Group_Internal'(Exclusive => True,
                                  Required  => False,
                                  Callback  => Base_Argument_Action'Access,
                                  others    => <>));
      declare
         Root_Group : constant Argument_Group := To_Unbounded_String ("_");
         Base_Group : constant Argument_Group :=
                        Parser.Add_Argument_Group
                          (Root_Group, "_base", Base_Argument_Action'Access);
         User_Group : constant Argument_Group :=
                        Parser.Add_Argument_Group
                          (Root_Group, "_root", null) with Unreferenced;
      begin
         Parser.Add_Argument (Base_Group, Help_Arg);
         Parser.Add_Argument (Base_Group, Version_Arg);
      end;

      return Parser;
   end Create;

   -------------
   -- Get_Opt --
   -------------

   procedure Get_Opt
     (Self      : Command_Line_Parser;
      From_Pack : GPR2.Package_Id;
      Values    : GPR2.Containers.Source_Value_List;
      Result    : in out Command_Line_Result'Class)
   is
      Index     : Positive := Values.First_Index;
      Next_Used : Boolean;
      State     : Parser_State;
   begin
      while Index <= Values.Last_Index loop
         Self.Get_Opt_Internal
           (Pack      => From_Pack,
            Arg       => String (Values.Element (Index).Text),
            Next      => (if Index = Values.Last_Index
                          then ""
                          else (String (Values.Element (Index + 1).Text))),
            Next_Used => Next_Used,
            State     => State,
            Result    => Result);
         if Next_Used then
            Index := Index + 2;
         else
            Index := Index + 1;
         end if;
      end loop;
   end Get_Opt;

   -------------
   -- Get_Opt --
   -------------

   procedure Get_Opt
     (Self   : Command_Line_Parser;
      Result : in out Command_Line_Result'Class)
   is
      Idx       : Natural := 1;
      State     : Parser_State;
      Next_Used : Boolean := False;

   begin
      while Idx <= Ada.Command_Line.Argument_Count loop
         Self.Get_Opt_Internal
           (Pack      => GPR2.Project_Level_Scope,
            Arg       => Ada.Command_Line.Argument (Idx),
            Next      => (if Idx = Ada.Command_Line.Argument_Count
                          then ""
                          else Ada.Command_Line.Argument (Idx + 1)),
            Next_Used => Next_Used,
            State     => State,
            Result    => Result);
         if Next_Used then
            Idx := Idx + 2;
         else
            Idx := Idx + 1;
         end if;
      end loop;
   end Get_Opt;

   ----------------------
   -- Get_Opt_Internal --
   ----------------------

   procedure Get_Opt_Internal
     (Parser    : Command_Line_Parser;
      Pack      : GPR2.Package_Id;
      Arg       : String;
      Next      : String;
      Next_Used : out Boolean;
      State     : in out Parser_State;
      Result    : in out Command_Line_Result'Class)
   is
      use type GPR2.Package_Id;

      function Find_Def (Sw : Switch_Type) return Arg_Maps.Cursor;

      --------------
      -- Find_Def --
      --------------

      function Find_Def (Sw : Switch_Type) return Arg_Maps.Cursor is
         Ret : Arg_Maps.Cursor;
         Alias_C : Switches_Maps.Cursor;
      begin
         Ret := Parser.Switches.Find (Sw);

         if not Arg_Maps.Has_Element (Ret) then
            Alias_C := Parser.Aliases.Find (Sw);

            if Switches_Maps.Has_Element (Alias_C) then
               Ret := Parser.Switches.Find (Switches_Maps.Element (Alias_C));
            end if;
         end if;

         return Ret;
      end Find_Def;

      Equ_Idx    : Natural := 0;
      Col_Idx    : Natural := 0;
      Found      : Boolean := False;
      Def        : Argument_Definition;
      Best_Match : Unbounded_String;
      Param      : Unbounded_String;
      Index      : Unbounded_String;
      C          : Arg_Maps.Cursor := Arg_Maps.No_Element;
      Is_Section : Boolean := False;

   begin
      Next_Used := False;

      if Arg'Length = 0 then
         return;
      end if;

      if Arg (Arg'First) = '-' then
         --  Look for known delimiters for index and parameter
         for J in Arg'Range loop
            if Col_Idx = 0 and then Arg (J) = ':' then
               Col_Idx := J;
            end if;
            if Equ_Idx = 0 and then Arg (J) = '=' then
               Equ_Idx := J;
               exit;
            end if;
         end loop;

         --  Lookup the switch definition
         if Col_Idx > 0 then
            C := Find_Def (Switch_Type (Arg (Arg'First .. Col_Idx - 1)));
         elsif Equ_Idx > 0 then
            C := Find_Def (Switch_Type (Arg (Arg'First .. Equ_Idx - 1)));
         else
            C := Find_Def (Switch_Type (Arg));
         end if;
      end if;

      if Arg_Maps.Has_Element (C) then
         Def := Arg_Maps.Element (C);

         if Pack /= GPR2.Project_Level_Scope
           and then not Def.In_Attr
         then
            --  Switch not available in the Switches Attribute
            raise Usage_Error with "option '" & Arg &
              "' not available in package " & GPR2.Image (Pack);
         end if;

         if Col_Idx > 0 then
            if Equ_Idx > 0 then
               Index := -Arg (Col_Idx + 1 .. Equ_Idx - 1);
               Param := -Arg (Equ_Idx + 1 .. Arg'Last);
            else
               Index := -Arg (Col_Idx + 1 .. Arg'Last);
            end if;
         elsif Equ_Idx > 0 then
            Param := -Arg (Equ_Idx + 1 .. Arg'Last);
         end if;

            --  Check for change of section

         if not Def.With_Value
           and then Def.Is_Section
         then
            if Def.Section_Callback = null then
               State.Current_Section := Null_Unbounded_String;
               State.Section_CB      := null;
            else
               State.Current_Section := Def.Name;
               State.Section_CB      := Def.Section_Callback;
            end if;

            Is_Section := True;
         end if;

         Found := True;

      else
         --  Look first for arguments with parameters that may not have
         --  delimiters, such as -XVAR=stuff or -jnn
         for Sw of Parser.Switches loop
            if Sw.With_Value
              and then Sw.Delimiter in None | Optional_Space
            then
               if Length (Sw.Name) < Arg'Length
                 and then Arg (Arg'First .. Arg'First + Length (Sw.Name) - 1)
                            = -Sw.Name
               then
                  --  Two switches may be ambiguous, for example -jnn and
                  --  -jcnn, in particular since the value is not typed
                  --  with this API (??? room for improvement here?). So
                  --  we continue until we found all ambiguous cases, and
                  --  pick up the best match (longest).

                  if not Found
                    or else Length (Def.Name) < Length (Sw.Name)
                  then
                     Best_Match := Sw.Name;
                     Def   := Sw;
                     Found := True;
                     Param := -Arg (Length (Sw.Name) + 1 .. Arg'Last);
                  end if;

               elsif Length (Sw.Alt_Name) > 0
                 and then Length (Sw.Alt_Name) < Arg'Length
                 and then Arg (Arg'First ..
                                 Arg'First + Length (Sw.Alt_Name) - 1)
                            = -Sw.Alt_Name
               then
                  --  See above

                  if not Found
                    or else Length (Best_Match) < Length (Sw.Alt_Name)
                  then
                     Best_Match := Sw.Alt_Name;
                     Def        := Sw;
                     Found      := True;
                     Param      :=
                       -Arg (Length (Sw.Alt_Name) + 1 .. Arg'Last);
                  end if;
               end if;
            end if;
         end loop;
      end if;

      if not Found
        and then Length (State.Current_Section) = 0
        and then Arg'Length > 0
      then
         if Arg (Arg'First) = '-' then
            raise Usage_Error with "unrecognized option '" & Arg & "'" &
              (if Pack = GPR2.Project_Level_Scope
               then ""
               else " in package " & GPR2.Image (Pack));
         end if;

         Result.Remaining.Append (Arg);
         --  ??? TODO: On windows, Arg may be a glob pattern: we
         --  need to manually expand it in this case !
         --  This is done by the shell on unix.
      end if;

      if Is_Section then
         State.Current_Section_Index := Index;

      elsif Length (State.Current_Section) > 0 then
         if Arg'Length > 0 then
            State.Section_CB
              (Parser, Result'Access,
               Section => -State.Current_Section,
               Index   => -State.Current_Section_Index,
               Arg     => Switch_Type (Arg));
         end if;

      elsif Found then
         if Length (Def.Index) = 0
           and then Length (Index) > 0
         then
            raise Usage_Error with
              "unexpected index for '" & Arg & "'" &
              (if Pack = GPR2.Project_Level_Scope
               then ""
               else " in package " & GPR2.Image (Pack));
         end if;

         if not Def.With_Value
           and then Length (Param) > 0
         then
            raise Usage_Error with
              "unexpected parameter for '" & Arg & "'" &
              (if Pack = GPR2.Project_Level_Scope
               then ""
               else " in package " & GPR2.Image (Pack));
         end if;

         if Def.With_Value
           and then Def.Delimiter = Space
           and then Length (Param) > 0
         then
            raise Usage_Error with
              "wrong format for '" & Arg & "'" &
              (if Pack = GPR2.Project_Level_Scope
               then ""
               else " in package " & GPR2.Image (Pack));
         end if;

         if Def.With_Value
           and then Length (Param) = 0
           and then Length (Def.Default) > 0
         then
            Param := Def.Default;
         end if;

         if Def.With_Value
           and then Length (Param) = 0
         then
            if Def.Delimiter /= None then
               Param := -Next;
               Next_Used := True;
            end if;
         end if;

         if Def.With_Value
           and then Length (Param) = 0
         then
            raise Usage_Error with
              "parameter expected for argument '" & Arg & "'" &
              (if Pack = GPR2.Project_Level_Scope
               then ""
               else " in package " & GPR2.Image (Pack));
         end if;

         Parser.Groups (GPR2.Name_Type (To_String (Def.Group))).Callback
           (Parser, Result'Access,
            Arg   => -Def.Name,
            Index => -Index,
            Param => -Param);
      end if;
   end Get_Opt_Internal;

   ------------
   -- Get_RO --
   ------------

   function Get_RO
     (Self  : Command_Line_Parser;
      Group : Argument_Group) return Group_Maps.Constant_Reference_Type is
   begin
      return Self.Groups.Constant_Reference
        (GPR2.Name_Type (To_String (Group)));
   end Get_RO;

   ------------
   -- Get_RW --
   ------------

   function Get_RW
     (Self  : in out Command_Line_Parser;
      Group : Argument_Group) return Group_Maps.Reference_Type is
   begin
      return Self.Groups.Reference (GPR2.Name_Type (To_String (Group)));
   end Get_RW;

   --------------
   -- Try_Help --
   --------------

   procedure Try_Help is
      use Ada.Text_IO;
   begin
      Put_Line
        (Standard_Error,
         "try """ & Get_Executable & " --help"" for more information.");
   end Try_Help;

   -----------
   -- Usage --
   -----------

   procedure Usage (Self : Command_Line_Parser) is
      function Short (Arg      : Argument_Definition;
                      Alt_Name : Boolean := False) return String;
      procedure Usage (Arg : Argument_Definition);
      procedure Usage (Group : Argument_Group);

      -----------
      -- Short --
      -----------

      function Short
        (Arg      : Argument_Definition;
         Alt_Name : Boolean := False) return String
      is
         Res : Unbounded_String := (if not Alt_Name
                                    then Arg.Name
                                    else Arg.Alt_Name);
      begin
         if Length (Arg.Index) > 0 then
            Append (Res, "[:" & (-Arg.Index) & "]");
         end if;

         if Arg.With_Value then
            if Length (Arg.Default) > 0 then
               Append (Res, "[");
            end if;

            case Arg.Delimiter is
               when None =>
                  null;
               when Space | Optional_Space =>
                  Append (Res, " ");
               when Equal =>
                  Append (Res, "=");
            end case;

            Append (Res, Arg.Parameter);

            if Length (Arg.Default) > 0 then
               Append (Res, "]");
            end if;
         elsif Arg.Is_Section then
            Append (Res, " opts");
         end if;

         return To_String (Res);
      end Short;

      -----------
      -- Usage --
      -----------

      procedure Usage (Arg : Argument_Definition) is
         Indent : constant String := "           ";
         Last   : Natural := 0;

      begin
         if Arg.Hidden then
            return;
         end if;

         declare
            Arg_Img : constant String := " " & Short (Arg) &
                                         (if Length (Arg.Alt_Name) > 0
                                          then ", " & Short (Arg, True)
                                          else "");
         begin
            Ada.Text_IO.Put (Arg_Img);

            if Length (Arg.Help) = 0 then
               Ada.Text_IO.New_Line;
               return;
            end if;

            if Arg_Img'Length + 2 > Indent'Length then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put (Indent);
            else
               declare
                  Empty : constant String (1 .. Indent'Length - Arg_Img'Length)
                            := (others => ' ');
               begin
                  Ada.Text_IO.Put (Empty);
               end;
            end if;
         end;

         loop
            if (79 - Indent'Length) >= (Length (Arg.Help) - Last) then
               Ada.Text_IO.Put_Line
                 (Slice (Arg.Help, Last + 1, Length (Arg.Help)));
               exit;
            else
               for J in reverse Last + 1 .. Last + 80 - Indent'Length loop
                  if Element (Arg.Help, J) = ' ' then
                     Ada.Text_IO.Put_Line
                       (Slice (Arg.Help, Last + 1, J - 1));
                     Last := J;
                     Ada.Text_IO.Put (Indent);

                     exit;
                  end if;
               end loop;
            end if;
         end loop;
      end Usage;

      procedure Usage (Group : Argument_Group) is
         Ref       : constant Group_Maps.Constant_Reference_Type :=
                       Self.Get_RO (Group);
         Show_Name : constant Boolean := Element (Group, 1) /= '_';
         Show_Help : constant Boolean := Length (Ref.Help) > 0;

      begin
         if Show_Name or else Show_Help then
            Ada.Text_IO.New_Line;
         end if;

         if Show_Name then
            Ada.Text_IO.Put_Line (To_String (Group) & " switches:");
         end if;

         if Show_Help then
            if Show_Name then
               Ada.Text_IO.Put ("   ");
            end if;

            Ada.Text_IO.Put (To_String (Ref.Help));

            if not Show_Name then
               Ada.Text_IO.Put (":");
            end if;

            Ada.Text_IO.New_Line;
         end if;

         if Show_Name and then Show_Help then
            Ada.Text_IO.New_Line;
         end if;

         for Arg of Ref.Switches loop
            Usage (Self.Switches.Element (Arg));
         end loop;

         for G of Ref.Subgroups loop
            Usage (To_Unbounded_String (String (G)));
         end loop;

         for G of Ref.Last_Subgroups loop
            Usage (To_Unbounded_String (String (G)));
         end loop;
      end Usage;

      Root : constant Argument_Group_Internal :=
               Self.Groups ("_");

   begin
      Ada.Text_IO.Put_Line ("Usage:");

      for Argument of Root.Switches loop
         Ada.Text_IO.Put_Line ("  " & (-Self.Tool) & " " &
                                 Short (Self.Switches.Element (Argument)));
      end loop;

      if Length (Self.Cmd_Line_Help) > 0 then
         Ada.Text_IO.Put_Line
           ("  " & (-Self.Tool) & " " & (-Self.Cmd_Line_Help));
      end if;

      Ada.Text_IO.New_Line;

      for Argument of Root.Switches loop
         Usage (Self.Switches.Element (Argument));
      end loop;

      for G of Root.Subgroups loop
         Usage (To_Unbounded_String (String (G)));
      end loop;
   end Usage;

   -------------
   -- Version --
   -------------

   procedure Version (Self : Command_Line_Parser) is
   begin
      GPR2.Version.Display (Ada.Characters.Handling.To_Upper (-Self.Tool),
                            -Self.Initial_Year,
                            GPR2.Version.Long_Value);
      GPR2.Version.Display_Free_Software;
   end Version;

end GPRtools.Command_Line;
