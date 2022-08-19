--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Attribute is

   ------------
   -- Create --
   ------------

   function Create
     (Index          : Attribute_Index.Object;
      Default_At_Pos : Unit_Index := No_Index) return Value_At_Pos
   is
      Is_Others : constant Boolean    :=
                    Index.Is_Defined and then Index.Is_Others;
      --  If we have the others index we prefix the string with '@' to
      --  avoid ambiguity with the "others" in quote which could be a
      --  language name.
      Value     : constant Value_Type :=
                    (if Is_Others then "@" else "")
                     & (if Index.Is_Defined
                        then Index.Value (Index.Is_Case_Sensitive)
                        else "");
   begin
      return V : Value_At_Pos (Value'Length) do
         V.Value  := Value;
         V.At_Pos := At_Pos_Or
                       (Source_Reference.Value.Object (Index), Default_At_Pos);
      end return;
   end Create;

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Index   : Attribute_Index.Object;
      Value   : Source_Reference.Value.Object;
      Default : Boolean := False;
      Frozen  : Boolean := False) return Object is
   begin
      return A : Object := Create (Name, Value) do
         A.Index   := Index;
         A.Default := Default;
         A.Frozen  := Frozen;
      end return;
   end Create;

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Index   : Attribute_Index.Object;
      Values  : Containers.Source_Value_List;
      Default : Boolean := False) return Object is
   begin
      return A : Object := Create (Name, Values) do
         A.Index   := Index;
         A.Default := Default;
      end return;
   end Create;

   overriding function Create
     (Name  : Source_Reference.Attribute.Object;
      Value : Source_Reference.Value.Object) return Object is
   begin
      return Object'
        (Attr_Values.Create (Name, Value)
         with others => <>);
   end Create;

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Value   : Source_Reference.Value.Object;
      Default : Boolean;
      Frozen  : Boolean := False) return Object is
   begin
      return Object'
        (Attr_Values.Create (Name, Value)
         with Default => Default,
              Frozen  => Frozen,
              others  => <>);
   end Create;

   overriding function Create
     (Name   : Source_Reference.Attribute.Object;
      Values : Containers.Source_Value_List) return Object is
   begin
      return Object'
        (Attr_Values.Create (Name, Values)
         with others => <>);
   end Create;

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Values  : Containers.Source_Value_List;
      Default : Boolean) return Object is
   begin
      return Object'
        (Attr_Values.Create (Name, Values)
         with Default => Default,
              others  => <>);
   end Create;

   function Create
     (Name                 : Q_Attribute_Id;
      Index                : Value_Type;
      Index_Case_Sensitive : Boolean;
      Source               : GPR2.Path_Name.Object;
      Default              : Value_Type;
      As_List              : Boolean) return Object
   is
   begin
      return Create
        (Name,
         Attribute_Index.Create (Index, Index_Case_Sensitive),
         Source, Default, As_List);
   end Create;

   function Create
     (Name    : Q_Attribute_Id;
      Index   : Attribute_Index.Object;
      Source  : GPR2.Path_Name.Object;
      Default : Value_Type;
      As_List : Boolean) return Object
   is
      SR_Name : constant Source_Reference.Attribute.Object :=
                  Source_Reference.Attribute.Object
                    (Source_Reference.Attribute.Create
                       (Filename => Source.Value,
                        Line     => 0,
                        Column   => 0,
                        Id       => Name));
      SR_Value : constant Source_Reference.Value.Object :=
                   Source_Reference.Value.Object
                     (Source_Reference.Value.Create
                        (Filename => Source.Value,
                         Line     => 0,
                         Column   => 0,
                         Text     => Default));
      SR_Values : GPR2.Containers.Source_Value_List;

   begin
      if As_List then
         SR_Values.Append (SR_Value);
         return Create
           (Name    => SR_Name,
            Index   => Index,
            Values  => SR_Values,
            Default => True);
      else
         return Project.Attribute.Create
           (Name    => SR_Name,
            Index   => Index,
            Value   => SR_Value,
            Default => True);
      end if;
   end Create;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Self : in out Object) is
   begin
      Self.Frozen := True;
   end Freeze;

   ---------------
   -- Get_Alias --
   ---------------

   function Get_Alias
     (Self     : Object;
      New_Name : Q_Attribute_Id) return Object
   is
      SR_Name : constant Source_Reference.Attribute.Object :=
                  Source_Reference.Attribute.Object
                    (Source_Reference.Attribute.Create
                       (Source_Reference.Object (Self), New_Name));
   begin
      return (Attr_Values.Object (Self).Rename (SR_Name) with
              Default     => Self.Default,
              Index       => Self.Index,
              Frozen      => Self.Frozen,
              Is_Alias    => True,
              From_Config => Self.From_Config);
   end Get_Alias;

   ---------------
   -- Has_Index --
   ---------------

   function Has_Index (Self : Object) return Boolean is
   begin
      return Self.Index.Is_Defined;
   end Has_Index;

   -----------
   -- Image --
   -----------

   function Image
     (Self     : Object;
      Name_Len : Natural := 0) return String
   is
      use GPR2.Project.Registry.Attribute;
      use all type GPR2.Project.Attr_Values.Object;

      Name   : constant String := Image (Self.Name.Id.Attr);
      Result : Unbounded_String := To_Unbounded_String ("for ");
   begin
      Append (Result, Name);

      if Name_Len > 0 and then Name'Length < Name_Len then
         Append (Result, (Name_Len - Name'Length) * ' ');
      end if;

      if Self.Has_Index then
         if Self.Index.Is_Others then
            Append (Result, " (others)");
         else
            Append (Result, " (""" & Self.Index.Text & """)");
         end if;
      end if;

      Append (Result, " use ");

      case Self.Kind is
         when Single =>
            Append (Result, '"' & Self.Value.Text & '"');

            if Self.Value.Has_At_Pos then
               Append (Result, " at" & Self.Value.At_Pos'Image);
            end if;

         when List =>
            Append (Result, Containers.Image (Self.Values));
      end case;

      Append (Result, ';');

      return To_String (Result);
   end Image;

   -----------
   -- Index --
   -----------

   function Index (Self : Object) return Attribute_Index.Object is
   begin
      return Self.Index;
   end Index;

   ------------
   -- Rename --
   ------------

   overriding function Rename
     (Self     : Object;
      Name     : Source_Reference.Attribute.Object) return Object is
   begin
      return (Attr_Values.Object (Self).Rename (Name) with
              Default     => True,
              Index       => Self.Index,
              Frozen      => Self.Frozen,
              Is_Alias    => False,
              From_Config => Self.From_Config);
   end Rename;

   --------------
   -- Set_Case --
   --------------

   procedure Set_Case
     (Self                    : in out Object;
      Index_Is_Case_Sensitive : Boolean;
      Value_Is_Case_Sensitive : Boolean) is
   begin
      Self.Set_Case (Value_Is_Case_Sensitive);

      if Self.Has_Index then
         Self.Index.Set_Case (Index_Is_Case_Sensitive);
      end if;
   end Set_Case;

   ----------------------
   -- Set_Default_Flag --
   ----------------------

   procedure Set_Default_Flag
     (Self : in out Object;
      Is_Default : Boolean) is
   begin
      Self.Default := Is_Default;
   end Set_Default_Flag;

   ---------------------
   -- Set_From_Config --
   ---------------------

   procedure Set_From_Config
     (Self : in out Object;
      From_Config : Boolean) is
   begin
      Self.From_Config := From_Config;
   end Set_From_Config;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index
     (Self  : in out Object;
      Index : Attribute_Index.Object)
   is
   begin
      Self.Index := Index;
   end Set_Index;

end GPR2.Project.Attribute;
