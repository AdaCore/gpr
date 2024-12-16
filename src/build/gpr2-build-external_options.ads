--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package is provided to simplify and normalize GPR common switches
--  support.

--  Once options object is configured (by parsing the command line or calling
--  On_Switch/Finalize functions directly), it can be used to load a Tree.

with GPR2.Containers;

with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Build.External_Options is

   type Action_Class is new String;
   Binder   : constant Action_Class := "binder";
   Compiler : constant Action_Class := "compiler";
   Linker   : constant Action_Class := "linker";

   type Object is tagged private;
   --  This object handles all external GPR switches.
   --  External GPR switches are switches that are not common between all GPR
   --  but still needs to be defined at the GPR library level to ensure a
   --  smooth process.
   --  Typical example are -cargs/-largs/-bargs which are GPRbuild command line
   --  switches but it makes sense to have if defined at GPR library level for
   --  the command line processing of actions.

   procedure Register (Self   : in out Object;
                       Action : Action_Class;
                       Lang   : Language_Id;
                       Option : String);
   --  Register an external option based on the external Name and Lang

   function Fetch (Self   : Object;
                   Action : Action_Class;
                   Lang   : Language_Id) return GPR2.Containers.Value_List;
   --  Fetch an external option based on the external Name and Lang

   procedure Clear (Self : in out Object);
   --  Clears the object

private

   package Lang_Value_List_Map_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Action_Class, GPR2.Containers.Lang_Value_List_Map, "<",
        GPR2.Containers.Lang_Value_List_Maps."=");
   subtype Lang_Value_List_Map_Map is Lang_Value_List_Map_Maps.Map;

   type Object is tagged record
      Ext_Opt : Lang_Value_List_Map_Map;
   end record;

end GPR2.Build.External_Options;
