--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Buffer;
with GNATCOLL.JSON;
with GNATCOLL.OS.FS;

with GPR2.Build.Actions;

package body GPR2.Build.Signature is

   package JSON renames GNATCOLL.JSON;
   package Buffer renames GNATCOLL.Buffer;

   function Add_Internal
     (Self           : in out Object;
      Art            : Artifacts.Object'Class;
      IO             : IO_Type;
      Checksum_Check : Boolean := True) return Boolean;

   ------------------------
   -- Add_Console_Output --
   ------------------------

   procedure Add_Console_Output
     (Self   : in out Object;
      Stdout : UB.Unbounded_String;
      Stderr : UB.Unbounded_String) is
   begin
      Self.Stdout := Stdout;
      Self.Stderr := Stderr;
   end Add_Console_Output;

   ---------------
   -- Add_Input --
   ---------------

   function Add_Input
     (Self           : in out Object;
      Art            : Artifacts.Object'Class;
      Checksum_Check : Boolean := True) return Boolean is
   begin
      return Add_Internal (Self, Art, Input, Checksum_Check);
   end Add_Input;

   ------------------
   -- Add_Internal --
   ------------------

   function Add_Internal
     (Self           : in out Object;
      Art            : Artifacts.Object'Class;
      IO             : IO_Type;
      Checksum_Check : Boolean := True) return Boolean
   is
      Added : Boolean;
      C     : Artifact_Sets.Cursor;
   begin
      Self.Artifacts (IO).Insert (Art, C, Added);

      if not Checksum_Check then
         return True;

      elsif not Added then
         --  Already there, so return False if the signature is already
         --  invalidated.
         return not Self.Checksums (IO).Is_Empty;

      elsif Self.Checksums (IO).Is_Empty then
         --  Nothing more to do, the signature is already invalidated
         return False;
      end if;

      --  Check immediately if the artifact checksum matches the saved state
      declare
         Chk : constant String := Art.Checksum;
         CC  : constant Checksum_Maps.Cursor := Self.Checksums (IO).Find (Art);
      begin
         if not Checksum_Maps.Has_Element (CC)
           or else Checksum_Maps.Element (CC) /= Chk
         then
            --  Invalidate the saved checksums
            Self.Checksums (Input).Clear;
            Self.Checksums (Output).Clear;

            return False;
         end if;
      end;

      return True;
   end Add_Internal;

   ----------------
   -- Add_Output --
   ----------------

   function Add_Output
     (Self : in out Object;
      Art  : Artifacts.Object'Class) return Boolean is
   begin
      return Add_Internal (Self, Art, Output);
   end Add_Output;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self := (others => <>);
   end Clear;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate (Self : in out Object) is
   begin
      for IO in Self.Checksums'Range loop
         Self.Checksums (IO).Clear;
      end loop;
   end Invalidate;

   ----------
   -- Load --
   ----------

   function Load (Db_File : Path_Name.Object;
                  Ctxt    : GPR2.Project.View.Object) return Object
   is
      use type JSON.JSON_Parser_Event_Kind;

      Signature   : Object;
      Parser      : JSON.JSON_Parser;
      Data        : Buffer.Reader := Buffer.Open (String (Db_File.Value));
      Event       : JSON.JSON_Parser_Event;

   begin
      --  Signature is an object
      Event := Parser.Parse_Next (Data => Data);
      if Event.Kind /= JSON.OBJECT_START then
         return Signature;
      end if;

      loop
         Event := Parser.Parse_Next (Data => Data);

         --  At this level we expect either object_end or a string value
         --  (the key of the object field).

         exit when Event.Kind = JSON.OBJECT_END;

         if Event.Kind /= JSON.STRING_VALUE then
            Signature.Clear;

            return Signature;
         end if;

         declare
            Key : String renames
                    Data.Token (Event.First + 1, Event.Last - 1);
            IO  : IO_Type := Input;
         begin

            if Key = TEXT_INPUTS or else  Key = TEXT_OUTPUTS then
               --  the signature key as an array associated with it
               Event := Parser.Parse_Next (Data => Data);

               if Event.Kind /= JSON.ARRAY_START then
                  return Undefined;
               end if;

               if Key = TEXT_INPUTS then
                  IO := Input;
               else
                  IO := Output;
               end if;

               Array_Loop : loop
                  Event := Parser.Parse_Next (Data => Data);
                  case Event.Kind is
                     when JSON.ARRAY_END =>
                        exit Array_Loop;
                     when JSON.OBJECT_START =>
                        declare
                           use JSON;
                           Protocol : JSON_Parser_Event := (NULL_VALUE, 0, 0);
                           Uri      : JSON_Parser_Event := (NULL_VALUE, 0, 0);
                           Value    : JSON_Parser_Event := (NULL_VALUE, 0, 0);
                           E        : JSON_Parser_Event;
                        begin
                           Item_Loop : loop
                              E := Parser.Parse_Next (Data);

                              if E.Kind = JSON.OBJECT_END then
                                 exit Item_Loop;
                              end if;

                              if E.Kind = JSON.STRING_VALUE then
                                 declare
                                    Key : constant String :=
                                            Data.Token
                                              (E.First + 1, E.Last - 1);
                                 begin
                                    if Key = TEXT_CLASS then
                                       Protocol := Parser.Parse_Next (Data);
                                    elsif Key = TEXT_KEY then
                                       Uri := Parser.Parse_Next (Data);
                                    elsif Key = TEXT_VALUE then
                                       Value := Parser.Parse_Next (Data);
                                    else
                                       return Undefined;
                                    end if;
                                 end;
                              else
                                 Signature.Clear;
                                 return Undefined;
                              end if;
                           end loop Item_Loop;

                           if Protocol.Kind /= STRING_VALUE
                             or else Uri.Kind /= STRING_VALUE
                             or else Value.Kind /= STRING_VALUE
                           then
                              return Undefined;
                           end if;

                           declare
                              Art : Artifacts.Object'Class :=
                                      Artifacts.New_Instance
                                        (Data.Token
                                           (Protocol.First + 1,
                                            Protocol.Last - 1));
                              Chk : constant String :=
                                       JSON.Decode_As_String (Value, Data);
                           begin
                              Art.Unserialize
                                (Data.Token (Uri.First + 1, Uri.Last - 1),
                                 Chk,
                                 Ctxt);

                              Signature.Checksums (IO).Include (Art, Chk);
                           end;
                        end;

                     when others =>
                        return Undefined;
                  end case;
               end loop Array_Loop;

            elsif Key = TEXT_STDOUT
              or else Key = TEXT_STDERR
            then
               Event := Parser.Parse_Next (Data => Data);

               if Event.Kind /= JSON.STRING_VALUE then
                  return Signature;
               end if;

               if Key = TEXT_STDOUT then
                  Signature.Stdout := To_Unbounded_String
                    (JSON.Decode_As_String (Event, Data));
               elsif Key = TEXT_STDERR then
                  Signature.Stderr := To_Unbounded_String
                    (JSON.Decode_As_String (Event, Data));
               end if;
            end if;
         end;
      end loop;

      --  Check for end of document
      Event := Parser.Parse_Next (Data => Data);
      if Event.Kind /= JSON.DOC_END then
         Signature.Clear;
         return Signature;
      end if;

      return Signature;

   exception
      when others =>
         Signature.Clear;
         return Signature;
   end Load;

   -----------
   -- Store --
   -----------

   procedure Store (Self : in out Object; Db_File : Path_Name.Object)
   is
      FD      : GNATCOLL.OS.FS.File_Descriptor;
      Value   : constant JSON.JSON_Value := JSON.Create_Object;
      Inputs  : JSON.JSON_Array;
      Outputs : JSON.JSON_Array;

      use type Ada.Containers.Count_Type;
      use GNATCOLL.OS.FS;

      function To_Artifact_Element
        (Position : Checksum_Maps.Cursor) return JSON.JSON_Value;

      function To_Artifact_Element
        (Position : Checksum_Maps.Cursor) return JSON.JSON_Value
      is
         Art : constant Artifacts.Object'Class :=
                 Checksum_Maps.Key (Position);
         Chk : constant String :=
                 Checksum_Maps.Element (Position);
         Val : constant JSON.JSON_Value := JSON.Create_Object;
      begin
         JSON.Set_Field (Val, TEXT_CLASS, Art.Protocol);
         JSON.Set_Field (Val, TEXT_KEY, Art.Serialize);
         JSON.Set_Field (Val, TEXT_VALUE, Chk);

         return Val;
      end To_Artifact_Element;

   begin
      for IO in IO_Type'Range loop
         if Self.Artifacts (IO).Length /= Self.Checksums (IO).Length then
            Self.Checksums (IO).Clear;

            for A of Self.Artifacts (IO) loop
               Self.Checksums (IO).Include (A, A.Checksum);
            end loop;
         end if;
      end loop;

      for C in Self.Checksums (Input).Iterate loop
         JSON.Append (Inputs, To_Artifact_Element (C));
      end loop;

      for C in Self.Checksums (Output).Iterate loop
         JSON.Append (Outputs, To_Artifact_Element (C));
      end loop;

      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_INPUTS,
                      Field      => Inputs);
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_OUTPUTS,
                      Field      => Outputs);
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_STDOUT,
                      Field      => Self.Stdout);
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_STDERR,
                      Field      => Self.Stderr);

      FD := Open (Db_File.String_Value, Write_Mode);

      if FD = Invalid_FD then
         raise GPR2.Build.Actions.Action_Error with
           "could not create file """ & Db_File.String_Value & '"';
      end if;

      Write (FD, JSON.Write (Value) & ASCII.CR & ASCII.LF);
      Close (FD);
   end Store;

   -----------
   -- Valid --
   -----------

   function Valid (Self : Object) return Boolean
   is
      use type Ada.Containers.Count_Type;
   begin
      return Self.Artifacts (Output).Length = Self.Checksums (Output).Length
        and then Self.Artifacts (Input).Length = Self.Checksums (Input).Length
        and then (not Self.Artifacts (Input).Is_Empty
                  or else not Self.Artifacts (Output).Is_Empty);
   end Valid;

end GPR2.Build.Signature;
