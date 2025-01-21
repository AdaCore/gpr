--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.JSON;
with GNATCOLL.Buffer;

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body GPR2.Build.Signature is

   package JSON renames GNATCOLL.JSON;
   package Buffer renames GNATCOLL.Buffer;

   ------------------
   -- Add_Artifact --
   ------------------

   procedure Add_Artifact
     (Self : in out Object;
      Art  : Build.Artifacts.Object'Class)
   is
      Chk : constant Hash_Digest := Art.Checksum;
   begin
      if Chk = No_Digest then
         Self.Has_Error := True;
      end if;

      Self.Artifacts.Include (Art, Chk);
   end Add_Artifact;

   ----------------
   -- Add_Output --
   ----------------

   procedure Add_Output
     (Self   : in out Object;
      Stdout : UB.Unbounded_String;
      Stderr : UB.Unbounded_String) is
   begin
      Self.Stdout := Stdout;
      Self.Stderr := Stderr;
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
      Self.Artifacts.Clear;
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
            Key : String renames Data.Token (Event.First + 1, Event.Last - 1);
         begin

            if Key = TEXT_SIGNATURE then
               --  the signature key as an array associated with it
               Event := Parser.Parse_Next (Data => Data);

               if Event.Kind /= JSON.ARRAY_START then
                  return Signature;
               end if;

               Array_Loop : loop
                  Event := Parser.Parse_Next (Data => Data);
                  case Event.Kind is
                     when JSON.ARRAY_END =>
                        exit Array_Loop;
                     when JSON.OBJECT_START =>
                        declare
                           use JSON;
                           Checksum : JSON_Parser_Event := (NULL_VALUE, 0, 0);
                           URI      : JSON_Parser_Event := (NULL_VALUE, 0, 0);
                           E        : JSON_Parser_Event;
                        begin
                           Checksum_Loop : loop
                              E := Parser.Parse_Next (Data);

                              if E.Kind = JSON.OBJECT_END then
                                 exit Checksum_Loop;
                              end if;

                              if E.Kind = JSON.STRING_VALUE then
                                 declare
                                    Key : constant String :=
                                      Data.Token (E.First + 1, E.Last - 1);
                                 begin
                                    if Key = TEXT_CHECKSUM then
                                       Checksum := Parser.Parse_Next (Data);
                                    elsif Key = TEXT_URI then
                                       URI := Parser.Parse_Next (Data);
                                    else
                                       Signature.Clear;
                                       return Signature;
                                    end if;
                                 end;
                              else
                                 Signature.Clear;
                                 return Signature;
                              end if;
                           end loop Checksum_Loop;

                           if Checksum.Kind /= STRING_VALUE or else
                             URI.Kind /= STRING_VALUE
                           then
                              Signature.Clear;
                              return Signature;
                           end if;

                           Signature.Artifacts.Include
                             (Build.Artifacts.From_Uri
                                (Data.Token
                                   (URI.First + 1,
                                      URI.Last - 1),
                                Ctxt),
                              Hash_Digest
                                (Data.Token
                                   (Checksum.First + 1,
                                    Checksum.Last - 1)));
                        end;
                     when others =>
                        Signature.Clear;
                        return Signature;
                  end case;
               end loop Array_Loop;

            elsif Key = TEXT_STDOUT
              or else Key = TEXT_STDERR
              or else Key = TEXT_CMDLINE
              or else Key = TEXT_CMDLINE_CHK
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
               elsif Key = TEXT_CMDLINE_CHK then
                  Signature.Cmd_Line_Checksum :=
                   Hash_Digest (Data.Token (Event.First + 1, Event.Last - 1));
               elsif Key = TEXT_CMDLINE then
                  Signature.Cmd_Line_Repr :=
                   +Data.Token (Event.First + 1, Event.Last - 1);
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

   ----------------------
   -- Missing_Artifact --
   ----------------------

   function Missing_Artifact
     (Self : Object) return GPR2.Build.Artifacts.Files.Object'Class is
   begin
      for C in Self.Artifacts.Iterate loop
         if Artifact_Maps.Element (C) = No_Digest then
            return GPR2.Build.Artifacts.Files.Object'Class
              (Artifact_Maps.Key (C));
         end if;
      end loop;

      raise Internal_Error with "Unexpected call to Missing_Artifact";
   end Missing_Artifact;

   -----------
   -- Store --
   -----------

   procedure Store (Self : in out Object; Db_File : Path_Name.Object) is
      File : File_Type;
      Value : constant JSON.JSON_Value := JSON.Create_Object;
      List : JSON.JSON_Array;

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor);

      procedure Create_Artifact_Element (Position : Artifact_Maps.Cursor) is
         Art : constant Build.Artifacts.Object'Class :=
                 Artifact_Maps.Key (Position);
         Val : constant JSON.JSON_Value := JSON.Create_Object;
      begin
         JSON.Set_Field (Val => Val,
                    Field_Name => TEXT_URI,
                    Field      => Build.Artifacts.To_Uri (Art));
         JSON.Set_Field (Val => Val,
                    Field_Name => TEXT_CHECKSUM,
                    Field      => String (Art.Checksum));
         JSON.Append (List, Val);
      end Create_Artifact_Element;
   begin
      Self.Artifacts.Iterate (Create_Artifact_Element'Access);

      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_SIGNATURE,
                      Field      => List);
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_STDOUT,
                      Field      => Self.Stdout);
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_STDERR,
                      Field      => Self.Stderr);
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_CMDLINE_CHK,
                      Field      => String (Self.Cmd_Line_Checksum));
      JSON.Set_Field (Val        => Value,
                      Field_Name => TEXT_CMDLINE,
                      Field      => -Self.Cmd_Line_Repr);

      if Exists (String (Db_File.Value)) then
         Open (File, Out_File, String (Db_File.Value));
         Put_Line (File, JSON.Write (Value) & ASCII.CR & ASCII.LF);
         Close (File);
      elsif Exists (String (Db_File.Containing_Directory.Value)) then
         Create (File, Out_File, String (Db_File.Value));
         Put_Line (File, JSON.Write (Value) & ASCII.CR & ASCII.LF);
         Close (File);
      end if;
   end Store;

   --------------------------------
   -- Update_Command_Line_Digest --
   --------------------------------

   procedure Update_Command_Line_Digest
     (Self : in out Object;
      Sig  : GPR2.Build.Command_Line.Object) is
   begin
      Self.Cmd_Line_Match := Self.Cmd_Line_Checksum = Sig.Checksum;

      if not Self.Cmd_Line_Match then
         Self.Cmd_Line_Checksum := Sig.Checksum;
         Self.Cmd_Line_Repr := +Sig.Signature;
      end if;
   end Update_Command_Line_Digest;

   -----------
   -- Valid --
   -----------

   function Valid (Self : Object) return Boolean is
   begin
      if not Self.Cmd_Line_Match then
         return False;
      end if;

      if Self.Artifacts.Is_Empty then
         return False;
      end if;

      for C in Self.Artifacts.Iterate loop
         if Artifact_Maps.Key (C).Checksum /= Artifact_Maps.Element (C) then
            return False;
         end if;
      end loop;

      return True;
   end Valid;

end GPR2.Build.Signature;
