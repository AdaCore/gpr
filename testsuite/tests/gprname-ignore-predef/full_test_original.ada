package Interfaces.Java is
   pragma Preelaborate;

   ---------------------
   -- Primitive types --
   ---------------------

   subtype Boolean is Standard.Boolean;

   type Char is new Wide_Character range
     Wide_Character'Val (16#00000000#) .. Wide_Character'Val (16#0000FFFF#);
   for Char'Size use 16;

   type Byte  is range -128 .. 127;
   for Byte'Size use  8;

   type Short is range -32768 .. 32767;
   for Short'Size use 16;

   type Int   is range -2147483648 .. 2147483647;
   for Int'Size use 32;

   type Long  is range -9223372036854775808 .. 9223372036854775807;
   for Long'Size use 64;

   type Float is new Standard.Float;
   --  TODO : this is not portable

   type Double is new Standard.Long_Float;
   --  TODO : this is not portable

end Interfaces.Java;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Awt.Color.ColorSpace is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new Java.Lang.Object.Typ with private;
   type ColorSpace is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type ColorSpace_Arr is access all Typ_Arr'Class;

   function New_ColorSpace_Arr (Length        : Standard.Interfaces.Java.Int;
                                Default_Value : ColorSpace := null)
                                return ColorSpace_Arr;

   function Get_ColorSpace (Arr      : access Typ_Arr;
                            Position : Standard.Interfaces.Java.Int)
                            return ColorSpace;

   procedure Set_ColorSpace (Arr      : access Typ_Arr;
                             Position : Standard.Interfaces.Java.Int;
                             Value    : ColorSpace);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type ColorSpace_Arr_2 is access all Typ_Arr_2'Class;

   function New_ColorSpace_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                                  Default_Value : ColorSpace_Arr := null)
                                  return ColorSpace_Arr_2;

   function Get_ColorSpace_Arr (Arr      : access Typ_Arr_2;
                                Position : Standard.Interfaces.Java.Int)
                                return ColorSpace_Arr;

   procedure Set_ColorSpace_Arr (Arr      : access Typ_Arr_2;
                                 Position : Standard.Interfaces.Java.Int;
                                 Value    : ColorSpace_Arr);

   ---------------------
   -- Field Accessors --
   ---------------------

   --  function Get_TYPE_XYZ return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_Lab return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_Luv return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_YCbCr return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_Yxy return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_RGB return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_GRAY return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_HSV return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_HLS return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_CMYK return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_CMY return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_2CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_3CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_4CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_5CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_6CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_7CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_8CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_9CLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_ACLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_BCLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_CCLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_DCLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_ECLR return Standard.Interfaces.Java.Int;

   --  function Get_TYPE_FCLR return Standard.Interfaces.Java.Int;

   --  function Get_CS_sRGB return Standard.Interfaces.Java.Int;

   --  function Get_CS_LINEAR_RGB return Standard.Interfaces.Java.Int;

   --  function Get_CS_CIEXYZ return Standard.Interfaces.Java.Int;

   --  function Get_CS_PYCC return Standard.Interfaces.Java.Int;

   --  function Get_CS_GRAY return Standard.Interfaces.Java.Int;

   -------------------------
   -- Method Declarations --
   -------------------------

   function FromCIEXYZ (This : access Typ;
                        P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                        return java.lang.Object.Object is abstract;

   function FromRGB (This : access Typ;
                     P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                     return java.lang.Object.Object is abstract;

   function GetInstance (P1_Int : Standard.Interfaces.Java.Int)
                         return java.lang.Object.Object;

   function GetName (This : access Typ;
                     P1_Int : Standard.Interfaces.Java.Int)
                     return java.lang.Object.Object;

   function GetNumComponents (This : access Typ)
                              return Standard.Interfaces.Java.Int;

   function GetType (This : access Typ)
                     return Standard.Interfaces.Java.Int;

   function IsCS_sRGB (This : access Typ)
                       return Standard.Interfaces.Java.Boolean;

   function ToCIEXYZ (This : access Typ;
                      P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                      return java.lang.Object.Object is abstract;

   function ToRGB (This : access Typ;
                   P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                   return java.lang.Object.Object is abstract;
private

   type Typ is abstract new Java.Lang.Object.Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.Color.ColorSpace;
limited with Java.Awt.Color.ColorSpace;
limited with Java.Awt.Image.ColorModel;
limited with Java.Lang.String;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Awt.Paint;
with Java.Lang.Object;

package Java.Awt.Color is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new Java.Lang.Object.Typ
     and Java.Awt.Paint.Typ with private;
   type Color is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type Color_Arr is access all Typ_Arr'Class;

   function New_Color_Arr (Length        : Standard.Interfaces.Java.Int;
                           Default_Value : Color := null)
                           return Color_Arr;

   function Get_Color (Arr      : access Typ_Arr;
                       Position : Standard.Interfaces.Java.Int)
                       return Color;

   procedure Set_Color (Arr      : access Typ_Arr;
                        Position : Standard.Interfaces.Java.Int;
                        Value    : Color);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Color_Arr_2 is access all Typ_Arr_2'Class;

   function New_Color_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                             Default_Value : Color_Arr := null)
                             return Color_Arr_2;

   function Get_Color_Arr (Arr      : access Typ_Arr_2;
                           Position : Standard.Interfaces.Java.Int)
                           return Color_Arr;

   procedure Set_Color_Arr (Arr      : access Typ_Arr_2;
                            Position : Standard.Interfaces.Java.Int;
                            Value    : Color_Arr);

   ---------------------
   -- Field Accessors --
   ---------------------

   --  function Get_White return java.lang.Object.Object;

   --  function Get_LightGray return java.lang.Object.Object;

   --  function Get_Gray return java.lang.Object.Object;

   --  function Get_DarkGray return java.lang.Object.Object;

   --  function Get_Black return java.lang.Object.Object;

   --  function Get_Red return java.lang.Object.Object;

   --  function Get_Pink return java.lang.Object.Object;

   --  function Get_Orange return java.lang.Object.Object;

   --  function Get_Yellow return java.lang.Object.Object;

   --  function Get_Green return java.lang.Object.Object;

   --  function Get_Magenta return java.lang.Object.Object;

   --  function Get_Cyan return java.lang.Object.Object;

   --  function Get_Blue return java.lang.Object.Object;

   ------------------------------
   -- Constructor Declarations --
   ------------------------------

   function New_Color (P1_Float : Standard.Interfaces.Java.Float;
                       P2_Float : Standard.Interfaces.Java.Float;
                       P3_Float : Standard.Interfaces.Java.Float)
                       return Color;

   function New_Color (P1_Float : Standard.Interfaces.Java.Float;
                       P2_Float : Standard.Interfaces.Java.Float;
                       P3_Float : Standard.Interfaces.Java.Float;
                       P4_Float : Standard.Interfaces.Java.Float)
                       return Color;

   function New_Color (P1_Int : Standard.Interfaces.Java.Int)
                       return Color;

   function New_Color (P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int)
                       return Color;

   function New_Color (P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int)
                       return Color;

   function New_Color (P1_Int : Standard.Interfaces.Java.Int;
                       P2_Boolean : Standard.Interfaces.Java.Boolean)
                       return Color;

   function New_Color (P1_ColorSpace : access Java.Awt.Color.ColorSpace.Typ'Class;
                       P2_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class;
                       P3_Float : Standard.Interfaces.Java.Float)
                       return Color;

   -------------------------
   -- Method Declarations --
   -------------------------

   function HSBtoRGB (P1_Float : Standard.Interfaces.Java.Float;
                      P2_Float : Standard.Interfaces.Java.Float;
                      P3_Float : Standard.Interfaces.Java.Float)
                      return Standard.Interfaces.Java.Int;

   function RGBtoHSB (P1_Int : Standard.Interfaces.Java.Int;
                      P2_Int : Standard.Interfaces.Java.Int;
                      P3_Int : Standard.Interfaces.Java.Int;
                      P4_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                      return java.lang.Object.Object;

   function Brighter (This : access Typ)
                      return java.lang.Object.Object;
--  synchronized

   function Darker (This : access Typ)
                    return java.lang.Object.Object;

   function Decode (P1_String : access Java.Lang.String.Typ'Class)
                    return java.lang.Object.Object;

   function Equals (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Boolean;

   function GetAlpha (This : access Typ)
                      return Standard.Interfaces.Java.Int;

   function GetBlue (This : access Typ)
                     return Standard.Interfaces.Java.Int;

   function GetColor (P1_String : access Java.Lang.String.Typ'Class)
                      return java.lang.Object.Object;

   function GetColor (P1_String : access Java.Lang.String.Typ'Class;
                      P2_Int : Standard.Interfaces.Java.Int)
                      return java.lang.Object.Object;

   function GetColor (P1_String : access Java.Lang.String.Typ'Class;
                      P2_Color : access Java.Awt.Color.Typ'Class)
                      return java.lang.Object.Object;

   function GetColorComponents (This : access Typ;
                                P1_ColorSpace : access Java.Awt.Color.ColorSpace.Typ'Class;
                                P2_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                                return java.lang.Object.Object;

   function GetColorComponents (This : access Typ;
                                P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                                return java.lang.Object.Object;

   function GetColorSpace (This : access Typ)
                           return java.lang.Object.Object;

   function GetComponents (This : access Typ;
                           P1_ColorSpace : access Java.Awt.Color.ColorSpace.Typ'Class;
                           P2_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                           return java.lang.Object.Object;

   function GetComponents (This : access Typ;
                           P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                           return java.lang.Object.Object;

   function GetGreen (This : access Typ)
                      return Standard.Interfaces.Java.Int;

   function GetHSBColor (P1_Float : Standard.Interfaces.Java.Float;
                         P2_Float : Standard.Interfaces.Java.Float;
                         P3_Float : Standard.Interfaces.Java.Float)
                         return java.lang.Object.Object;

   function GetRGB (This : access Typ)
                    return Standard.Interfaces.Java.Int;

   function GetRGBColorComponents (This : access Typ;
                                   P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                                   return java.lang.Object.Object;

   function GetRGBComponents (This : access Typ;
                              P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                              return java.lang.Object.Object;

   function GetRed (This : access Typ)
                    return Standard.Interfaces.Java.Int;

   function GetTransparency (This : access Typ)
                             return Standard.Interfaces.Java.Int;

   function HashCode (This : access Typ)
                      return Standard.Interfaces.Java.Int;

   function ToString (This : access Typ)
                      return java.lang.Object.Object;
private

   type Typ is new Java.Lang.Object.Typ
     and Java.Awt.Paint.Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.Color;
limited with Java.Awt.Color;
limited with Java.Awt.Image;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Awt.Graphics is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new Java.Lang.Object.Typ with private;
   type Graphics is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type Graphics_Arr is access all Typ_Arr'Class;

   function New_Graphics_Arr (Length        : Standard.Interfaces.Java.Int;
                              Default_Value : Graphics := null)
                              return Graphics_Arr;

   function Get_Graphics (Arr      : access Typ_Arr;
                          Position : Standard.Interfaces.Java.Int)
                          return Graphics;

   procedure Set_Graphics (Arr      : access Typ_Arr;
                           Position : Standard.Interfaces.Java.Int;
                           Value    : Graphics);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Graphics_Arr_2 is access all Typ_Arr_2'Class;

   function New_Graphics_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                                Default_Value : Graphics_Arr := null)
                                return Graphics_Arr_2;

   function Get_Graphics_Arr (Arr      : access Typ_Arr_2;
                              Position : Standard.Interfaces.Java.Int)
                              return Graphics_Arr;

   procedure Set_Graphics_Arr (Arr      : access Typ_Arr_2;
                               Position : Standard.Interfaces.Java.Int;
                               Value    : Graphics_Arr);

   -------------------------
   -- Method Declarations --
   -------------------------

   procedure ClearRect (This : access Typ;
                        P1_Int : Standard.Interfaces.Java.Int;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int;
                        P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure ClipRect (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure CopyArea (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int;
                       P5_Int : Standard.Interfaces.Java.Int;
                       P6_Int : Standard.Interfaces.Java.Int) is abstract;

   function Create (This : access Typ)
                    return java.lang.Object.Object is abstract;

   function Create (This : access Typ;
                    P1_Int : Standard.Interfaces.Java.Int;
                    P2_Int : Standard.Interfaces.Java.Int;
                    P3_Int : Standard.Interfaces.Java.Int;
                    P4_Int : Standard.Interfaces.Java.Int)
                    return java.lang.Object.Object;

   procedure Dispose (This : access Typ) is abstract;

   procedure Draw3DRect (This : access Typ;
                         P1_Int : Standard.Interfaces.Java.Int;
                         P2_Int : Standard.Interfaces.Java.Int;
                         P3_Int : Standard.Interfaces.Java.Int;
                         P4_Int : Standard.Interfaces.Java.Int;
                         P5_Boolean : Standard.Interfaces.Java.Boolean);

   procedure DrawArc (This : access Typ;
                      P1_Int : Standard.Interfaces.Java.Int;
                      P2_Int : Standard.Interfaces.Java.Int;
                      P3_Int : Standard.Interfaces.Java.Int;
                      P4_Int : Standard.Interfaces.Java.Int;
                      P5_Int : Standard.Interfaces.Java.Int;
                      P6_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure DrawBytes (This : access Typ;
                        P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int;
                        P4_Int : Standard.Interfaces.Java.Int;
                        P5_Int : Standard.Interfaces.Java.Int);

   procedure DrawChars (This : access Typ;
                        P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int;
                        P4_Int : Standard.Interfaces.Java.Int;
                        P5_Int : Standard.Interfaces.Java.Int);

   function DrawImage (This : access Typ;
                       P1_Image : access Java.Awt.Image.Typ'Class;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int;
                       P5_Int : Standard.Interfaces.Java.Int;
                       P6_Int : Standard.Interfaces.Java.Int;
                       P7_Int : Standard.Interfaces.Java.Int;
                       P8_Int : Standard.Interfaces.Java.Int;
                       P9_Int : Standard.Interfaces.Java.Int;
                       P10_Color : access Java.Awt.Color.Typ'Class;
                       P11_ImageObserver : access Java.Awt.Image.ImageObserver.Typ'Class)
                       return Standard.Interfaces.Java.Boolean is abstract;

   function DrawImage (This : access Typ;
                       P1_Image : access Java.Awt.Image.Typ'Class;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int;
                       P5_Int : Standard.Interfaces.Java.Int;
                       P6_Int : Standard.Interfaces.Java.Int;
                       P7_Int : Standard.Interfaces.Java.Int;
                       P8_Int : Standard.Interfaces.Java.Int;
                       P9_Int : Standard.Interfaces.Java.Int;
                       P10_ImageObserver : access Java.Awt.Image.ImageObserver.Typ'Class)
                       return Standard.Interfaces.Java.Boolean is abstract;

   function DrawImage (This : access Typ;
                       P1_Image : access Java.Awt.Image.Typ'Class;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int;
                       P5_Int : Standard.Interfaces.Java.Int;
                       P6_Color : access Java.Awt.Color.Typ'Class;
                       P7_ImageObserver : access Java.Awt.Image.ImageObserver.Typ'Class)
                       return Standard.Interfaces.Java.Boolean is abstract;

   function DrawImage (This : access Typ;
                       P1_Image : access Java.Awt.Image.Typ'Class;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int;
                       P5_Int : Standard.Interfaces.Java.Int;
                       P6_ImageObserver : access Java.Awt.Image.ImageObserver.Typ'Class)
                       return Standard.Interfaces.Java.Boolean is abstract;

   function DrawImage (This : access Typ;
                       P1_Image : access Java.Awt.Image.Typ'Class;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Color : access Java.Awt.Color.Typ'Class;
                       P5_ImageObserver : access Java.Awt.Image.ImageObserver.Typ'Class)
                       return Standard.Interfaces.Java.Boolean is abstract;

   function DrawImage (This : access Typ;
                       P1_Image : access Java.Awt.Image.Typ'Class;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_ImageObserver : access Java.Awt.Image.ImageObserver.Typ'Class)
                       return Standard.Interfaces.Java.Boolean is abstract;

   procedure DrawLine (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure DrawOval (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure DrawPolyline (This : access Typ;
                           P1_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                           P2_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                           P3_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure DrawRect (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int);

   procedure DrawRoundRect (This : access Typ;
                            P1_Int : Standard.Interfaces.Java.Int;
                            P2_Int : Standard.Interfaces.Java.Int;
                            P3_Int : Standard.Interfaces.Java.Int;
                            P4_Int : Standard.Interfaces.Java.Int;
                            P5_Int : Standard.Interfaces.Java.Int;
                            P6_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure DrawString (This : access Typ;
                         P1_String : access Java.Lang.String.Typ'Class;
                         P2_Int : Standard.Interfaces.Java.Int;
                         P3_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure Fill3DRect (This : access Typ;
                         P1_Int : Standard.Interfaces.Java.Int;
                         P2_Int : Standard.Interfaces.Java.Int;
                         P3_Int : Standard.Interfaces.Java.Int;
                         P4_Int : Standard.Interfaces.Java.Int;
                         P5_Boolean : Standard.Interfaces.Java.Boolean);

   procedure FillArc (This : access Typ;
                      P1_Int : Standard.Interfaces.Java.Int;
                      P2_Int : Standard.Interfaces.Java.Int;
                      P3_Int : Standard.Interfaces.Java.Int;
                      P4_Int : Standard.Interfaces.Java.Int;
                      P5_Int : Standard.Interfaces.Java.Int;
                      P6_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure FillOval (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure FillPolygon (This : access Typ;
                          P1_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                          P2_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                          P3_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure FillRect (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure FillRoundRect (This : access Typ;
                            P1_Int : Standard.Interfaces.Java.Int;
                            P2_Int : Standard.Interfaces.Java.Int;
                            P3_Int : Standard.Interfaces.Java.Int;
                            P4_Int : Standard.Interfaces.Java.Int;
                            P5_Int : Standard.Interfaces.Java.Int;
                            P6_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure Finalize (This : access Typ);

   function GetClip (This : access Typ)
                     return java.lang.Object.Object is abstract;

   function GetClipBounds (This : access Typ)
                           return java.lang.Object.Object is abstract;

   function GetClipRect (This : access Typ)
                         return java.lang.Object.Object;

   function GetColor (This : access Typ)
                      return java.lang.Object.Object is abstract;

   function GetFont (This : access Typ)
                     return java.lang.Object.Object is abstract;

   function GetFontMetrics (This : access Typ)
                            return java.lang.Object.Object;

   function HitClip (This : access Typ;
                     P1_Int : Standard.Interfaces.Java.Int;
                     P2_Int : Standard.Interfaces.Java.Int;
                     P3_Int : Standard.Interfaces.Java.Int;
                     P4_Int : Standard.Interfaces.Java.Int)
                     return Standard.Interfaces.Java.Boolean;

   procedure SetClip (This : access Typ;
                      P1_Int : Standard.Interfaces.Java.Int;
                      P2_Int : Standard.Interfaces.Java.Int;
                      P3_Int : Standard.Interfaces.Java.Int;
                      P4_Int : Standard.Interfaces.Java.Int) is abstract;

   procedure SetColor (This : access Typ;
                       P1_Color : access Java.Awt.Color.Typ'Class) is abstract;

   procedure SetPaintMode (This : access Typ) is abstract;

   procedure SetXORMode (This : access Typ;
                         P1_Color : access Java.Awt.Color.Typ'Class) is abstract;

   function ToString (This : access Typ)
                      return java.lang.Object.Object;

   procedure Translate (This : access Typ;
                        P1_Int : Standard.Interfaces.Java.Int;
                        P2_Int : Standard.Interfaces.Java.Int) is abstract;
private

   type Typ is abstract new Java.Lang.Object.Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.Graphics;
limited with Java.Awt.Color.ColorSpace;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Awt.Image.ColorModel is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new Java.Lang.Object.Typ with private;
   type ColorModel is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type ColorModel_Arr is access all Typ_Arr'Class;

   function New_ColorModel_Arr (Length        : Standard.Interfaces.Java.Int;
                                Default_Value : ColorModel := null)
                                return ColorModel_Arr;

   function Get_ColorModel (Arr      : access Typ_Arr;
                            Position : Standard.Interfaces.Java.Int)
                            return ColorModel;

   procedure Set_ColorModel (Arr      : access Typ_Arr;
                             Position : Standard.Interfaces.Java.Int;
                             Value    : ColorModel);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type ColorModel_Arr_2 is access all Typ_Arr_2'Class;

   function New_ColorModel_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                                  Default_Value : ColorModel_Arr := null)
                                  return ColorModel_Arr_2;

   function Get_ColorModel_Arr (Arr      : access Typ_Arr_2;
                                Position : Standard.Interfaces.Java.Int)
                                return ColorModel_Arr;

   procedure Set_ColorModel_Arr (Arr      : access Typ_Arr_2;
                                 Position : Standard.Interfaces.Java.Int;
                                 Value    : ColorModel_Arr);

   ---------------------
   -- Field Accessors --
   ---------------------

   --  protected
   --  function Get_Pixel_bits(this : access Typ)
   --  return Standard.Interfaces.Java.Int;

   --  procedure Set_Pixel_bits (this : access Typ; --  F : Standard.Interfaces.Java.Int);

   --  protected
   --  function Get_TransferType(this : access Typ)
   --  return Standard.Interfaces.Java.Int;

   --  procedure Set_TransferType (this : access Typ; --  F : Standard.Interfaces.Java.Int);

   -------------------------
   -- Method Declarations --
   -------------------------

   function CreateCompatibleSampleModel (This : access Typ;
                                         P1_Int : Standard.Interfaces.Java.Int;
                                         P2_Int : Standard.Interfaces.Java.Int)
                                         return java.lang.Object.Object;

   function CreateCompatibleWritableRaster (This : access Typ;
                                            P1_Int : Standard.Interfaces.Java.Int;
                                            P2_Int : Standard.Interfaces.Java.Int)
                                            return java.lang.Object.Object;

   function Equals (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Boolean;

   procedure Finalize (This : access Typ);

   function GetAlpha (This : access Typ;
                      P1_Int : Standard.Interfaces.Java.Int)
                      return Standard.Interfaces.Java.Int is abstract;

   function GetAlpha (This : access Typ;
                      P1_Object : access Java.Lang.Object.Typ'Class)
                      return Standard.Interfaces.Java.Int;

   function GetBlue (This : access Typ;
                     P1_Int : Standard.Interfaces.Java.Int)
                     return Standard.Interfaces.Java.Int is abstract;

   function GetBlue (This : access Typ;
                     P1_Object : access Java.Lang.Object.Typ'Class)
                     return Standard.Interfaces.Java.Int;
--  final

   function GetColorSpace (This : access Typ)
                           return java.lang.Object.Object;

   function GetComponentSize (This : access Typ)
                              return java.lang.Object.Object;

   function GetComponentSize (This : access Typ;
                              P1_Int : Standard.Interfaces.Java.Int)
                              return Standard.Interfaces.Java.Int;

   function GetComponents (This : access Typ;
                           P1_Int : Standard.Interfaces.Java.Int;
                           P2_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                           P3_Int : Standard.Interfaces.Java.Int)
                           return java.lang.Object.Object;

   function GetComponents (This : access Typ;
                           P1_Object : access Java.Lang.Object.Typ'Class;
                           P2_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                           P3_Int : Standard.Interfaces.Java.Int)
                           return java.lang.Object.Object;

   function GetDataElement (This : access Typ;
                            P1_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                            P2_Int : Standard.Interfaces.Java.Int)
                            return Standard.Interfaces.Java.Int;

   function GetDataElements (This : access Typ;
                             P1_Int : Standard.Interfaces.Java.Int;
                             P2_Object : access Java.Lang.Object.Typ'Class)
                             return java.lang.Object.Object;

   function GetDataElements (This : access Typ;
                             P1_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                             P2_Int : Standard.Interfaces.Java.Int;
                             P3_Object : access Java.Lang.Object.Typ'Class)
                             return java.lang.Object.Object;

   function GetGreen (This : access Typ;
                      P1_Int : Standard.Interfaces.Java.Int)
                      return Standard.Interfaces.Java.Int is abstract;

   function GetGreen (This : access Typ;
                      P1_Object : access Java.Lang.Object.Typ'Class)
                      return Standard.Interfaces.Java.Int;

   function GetNormalizedComponents (This : access Typ;
                                     P1_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                                     P2_Int : Standard.Interfaces.Java.Int;
                                     P3_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class;
                                     P4_Int : Standard.Interfaces.Java.Int)
                                     return java.lang.Object.Object;

   function GetNumColorComponents (This : access Typ)
                                   return Standard.Interfaces.Java.Int;

   function GetNumComponents (This : access Typ)
                              return Standard.Interfaces.Java.Int;

   function GetPixelSize (This : access Typ)
                          return Standard.Interfaces.Java.Int;

   function GetRGB (This : access Typ;
                    P1_Int : Standard.Interfaces.Java.Int)
                    return Standard.Interfaces.Java.Int;

   function GetRGB (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Int;

   function GetRGBdefault return java.lang.Object.Object;

   function GetRed (This : access Typ;
                    P1_Int : Standard.Interfaces.Java.Int)
                    return Standard.Interfaces.Java.Int is abstract;

   function GetRed (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Int;

   function GetTransparency (This : access Typ)
                             return Standard.Interfaces.Java.Int;

   function GetUnnormalizedComponents (This : access Typ;
                                       P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class;
                                       P2_Int : Standard.Interfaces.Java.Int;
                                       P3_Int_Arr : access Java_Arrays.Int_Arr_Typ'Class;
                                       P4_Int : Standard.Interfaces.Java.Int)
                                       return java.lang.Object.Object;
--  final

   function HasAlpha (This : access Typ)
                      return Standard.Interfaces.Java.Boolean;
--  final

   function IsAlphaPremultiplied (This : access Typ)
                                  return Standard.Interfaces.Java.Boolean;

   function ToString (This : access Typ)
                      return java.lang.Object.Object;
private

   type Typ is abstract new Java.Lang.Object.Typ with null record;


   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.Image.ColorModel;
limited with Java.Awt.Graphics;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Awt.Image is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new Java.Lang.Object.Typ with private;
   type Image is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type Image_Arr is access all Typ_Arr'Class;

   function New_Image_Arr (Length        : Standard.Interfaces.Java.Int;
                           Default_Value : Image := null)
                           return Image_Arr;

   function Get_Image (Arr      : access Typ_Arr;
                       Position : Standard.Interfaces.Java.Int)
                       return Image;

   procedure Set_Image (Arr      : access Typ_Arr;
                        Position : Standard.Interfaces.Java.Int;
                        Value    : Image);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Image_Arr_2 is access all Typ_Arr_2'Class;

   function New_Image_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                             Default_Value : Image_Arr := null)
                             return Image_Arr_2;

   function Get_Image_Arr (Arr      : access Typ_Arr_2;
                           Position : Standard.Interfaces.Java.Int)
                           return Image_Arr;

   procedure Set_Image_Arr (Arr      : access Typ_Arr_2;
                            Position : Standard.Interfaces.Java.Int;
                            Value    : Image_Arr);

   ---------------------
   -- Field Accessors --
   ---------------------

   --  function Get_UndefinedProperty return java.lang.Object.Object;

   --  function Get_SCALE_DEFAULT return Standard.Interfaces.Java.Int;

   --  function Get_SCALE_FAST return Standard.Interfaces.Java.Int;

   --  function Get_SCALE_SMOOTH return Standard.Interfaces.Java.Int;

   --  function Get_SCALE_REPLICATE return Standard.Interfaces.Java.Int;

   --  function Get_SCALE_AREA_AVERAGING return Standard.Interfaces.Java.Int;

   -------------------------
   -- Method Declarations --
   -------------------------

   procedure Flush (This : access Typ) is abstract;

   function GetGraphics (This : access Typ)
                         return java.lang.Object.Object is abstract;

   function GetScaledInstance (This : access Typ;
                               P1_Int : Standard.Interfaces.Java.Int;
                               P2_Int : Standard.Interfaces.Java.Int;
                               P3_Int : Standard.Interfaces.Java.Int)
                               return java.lang.Object.Object;

   function GetSource (This : access Typ)
                       return java.lang.Object.Object is abstract;

private

   type Typ is abstract new Java.Lang.Object.Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.Image;
limited with Java.Awt.RenderingHints;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Awt.Paint is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is Interface;
   type Paint is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type Paint_Arr is access all Typ_Arr'Class;

   function New_Paint_Arr (Length        : Standard.Interfaces.Java.Int;
                           Default_Value : Paint := null)
                           return Paint_Arr;

   function Get_Paint (Arr      : access Typ_Arr;
                       Position : Standard.Interfaces.Java.Int)
                       return Paint;

   procedure Set_Paint (Arr      : access Typ_Arr;
                        Position : Standard.Interfaces.Java.Int;
                        Value    : Paint);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Paint_Arr_2 is access all Typ_Arr_2'Class;

   function New_Paint_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                             Default_Value : Paint_Arr := null)
                             return Paint_Arr_2;

   function Get_Paint_Arr (Arr      : access Typ_Arr_2;
                           Position : Standard.Interfaces.Java.Int)
                           return Paint_Arr;

   procedure Set_Paint_Arr (Arr      : access Typ_Arr_2;
                            Position : Standard.Interfaces.Java.Int;
                            Value    : Paint_Arr);

private

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.Paint;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Awt.RenderingHints is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new Java.Lang.Object.Typ with private;
   type RenderingHints is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type RenderingHints_Arr is access all Typ_Arr'Class;

   function New_RenderingHints_Arr (Length        : Standard.Interfaces.Java.Int;
                                    Default_Value : RenderingHints := null)
                                    return RenderingHints_Arr;

   function Get_RenderingHints (Arr      : access Typ_Arr;
                                Position : Standard.Interfaces.Java.Int)
                                return RenderingHints;

   procedure Set_RenderingHints (Arr      : access Typ_Arr;
                                 Position : Standard.Interfaces.Java.Int;
                                 Value    : RenderingHints);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type RenderingHints_Arr_2 is access all Typ_Arr_2'Class;

   function New_RenderingHints_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                                      Default_Value : RenderingHints_Arr := null)
                                      return RenderingHints_Arr_2;

   function Get_RenderingHints_Arr (Arr      : access Typ_Arr_2;
                                    Position : Standard.Interfaces.Java.Int)
                                    return RenderingHints_Arr;

   procedure Set_RenderingHints_Arr (Arr      : access Typ_Arr_2;
                                     Position : Standard.Interfaces.Java.Int;
                                     Value    : RenderingHints_Arr);

   ---------------------
   -- Field Accessors --
   ---------------------

   --  function Get_KEY_ANTIALIASING return java.lang.Object.Object;

   --  function Get_VALUE_ANTIALIAS_ON return java.lang.Object.Object;

   --  function Get_VALUE_ANTIALIAS_OFF return java.lang.Object.Object;

   --  function Get_VALUE_ANTIALIAS_DEFAULT return java.lang.Object.Object;

   --  function Get_KEY_RENDERING return java.lang.Object.Object;

   --  function Get_VALUE_RENDER_SPEED return java.lang.Object.Object;

   --  function Get_VALUE_RENDER_QUALITY return java.lang.Object.Object;

   --  function Get_VALUE_RENDER_DEFAULT return java.lang.Object.Object;

   --  function Get_KEY_DITHERING return java.lang.Object.Object;

   --  function Get_VALUE_DITHER_DISABLE return java.lang.Object.Object;

   --  function Get_VALUE_DITHER_ENABLE return java.lang.Object.Object;

   --  function Get_VALUE_DITHER_DEFAULT return java.lang.Object.Object;

   --  function Get_KEY_TEXT_ANTIALIASING return java.lang.Object.Object;

   --  function Get_VALUE_TEXT_ANTIALIAS_ON return java.lang.Object.Object;

   --  function Get_VALUE_TEXT_ANTIALIAS_OFF return java.lang.Object.Object;

   --  function Get_VALUE_TEXT_ANTIALIAS_DEFAULT return java.lang.Object.Object;

   --  function Get_KEY_FRACTIONALMETRICS return java.lang.Object.Object;

   --  function Get_VALUE_FRACTIONALMETRICS_OFF return java.lang.Object.Object;

   --  function Get_VALUE_FRACTIONALMETRICS_ON return java.lang.Object.Object;

   --  function Get_VALUE_FRACTIONALMETRICS_DEFAULT return java.lang.Object.Object;

   --  function Get_KEY_INTERPOLATION return java.lang.Object.Object;

   --  function Get_VALUE_INTERPOLATION_NEAREST_NEIGHBOR return java.lang.Object.Object;

   --  function Get_VALUE_INTERPOLATION_BILINEAR return java.lang.Object.Object;

   --  function Get_VALUE_INTERPOLATION_BICUBIC return java.lang.Object.Object;

   --  function Get_KEY_ALPHA_INTERPOLATION return java.lang.Object.Object;

   --  function Get_VALUE_ALPHA_INTERPOLATION_SPEED return java.lang.Object.Object;

   --  function Get_VALUE_ALPHA_INTERPOLATION_QUALITY return java.lang.Object.Object;

   --  function Get_VALUE_ALPHA_INTERPOLATION_DEFAULT return java.lang.Object.Object;

   --  function Get_KEY_COLOR_RENDERING return java.lang.Object.Object;

   --  function Get_VALUE_COLOR_RENDER_SPEED return java.lang.Object.Object;

   --  function Get_VALUE_COLOR_RENDER_QUALITY return java.lang.Object.Object;

   --  function Get_VALUE_COLOR_RENDER_DEFAULT return java.lang.Object.Object;

   -------------------------
   -- Method Declarations --
   -------------------------

   procedure Add (This : access Typ;
                  P1_RenderingHints : access Java.Awt.RenderingHints.Typ'Class);

   procedure Clear (This : access Typ);

   function Clone (This : access Typ)
                   return java.lang.Object.Object;

   function ContainsKey (This : access Typ;
                         P1_Object : access Java.Lang.Object.Typ'Class)
                         return Standard.Interfaces.Java.Boolean;

   function ContainsValue (This : access Typ;
                           P1_Object : access Java.Lang.Object.Typ'Class)
                           return Standard.Interfaces.Java.Boolean;

   function EntrySet (This : access Typ)
                      return java.lang.Object.Object;

   function Equals (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Boolean;

   function Get (This : access Typ;
                 P1_Object : access Java.Lang.Object.Typ'Class)
                 return java.lang.Object.Object;

   function HashCode (This : access Typ)
                      return Standard.Interfaces.Java.Int;

   function IsEmpty (This : access Typ)
                     return Standard.Interfaces.Java.Boolean;

   function KeySet (This : access Typ)
                    return java.lang.Object.Object;

   function Put (This : access Typ;
                 P1_Object : access Java.Lang.Object.Typ'Class;
                 P2_Object : access Java.Lang.Object.Typ'Class)
                 return java.lang.Object.Object;

   function Remove (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return java.lang.Object.Object;

   function Size (This : access Typ)
                  return Standard.Interfaces.Java.Int;

   function ToString (This : access Typ)
                      return java.lang.Object.Object;

   function Values (This : access Typ)
                    return java.lang.Object.Object;
private

   type Typ is new Java.Lang.Object.Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Awt.RenderingHints;
package Java.Awt is
end Java.Awt;
limited with Java_Arrays;
with Interfaces.Java;
with JNI; use JNI;
with JNI_Object; use JNI_Object;

package Java.Lang.Object is

   -----------------------
   -- Type Declarations --
   -----------------------

   type Typ is abstract new JNI_Data with private;
   type Object is access all Typ'Class;

   ---------------------
   -- Root Array Type --
   ---------------------

   --  All array types are descendant of Root_Array.
   --  This allows to share the implementation of Get_Length
   --  which is exactly the same for each array type
   type Root_Array is abstract new Typ with private;

   function Get_Length (Arr : access Root_Array)
                        return Standard.Interfaces.Java.Int;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type Object_Arr is access all Typ_Arr'Class;

   function New_Object_Arr (Length        : Standard.Interfaces.Java.Int;
                            Default_Value : Object := null)
                            return Object_Arr;

   function Get_Object (Arr      : access Typ_Arr;
                        Position : Standard.Interfaces.Java.Int)
                        return Object;

   procedure Set_Object (Arr      : access Typ_Arr;
                         Position : Standard.Interfaces.Java.Int;
                         Value    : Object);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Object_Arr_2 is access all Typ_Arr_2'Class;

   function New_Object_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                              Default_Value : Object_Arr := null)
                              return Object_Arr_2;

   function Get_Object_Arr (Arr      : access Typ_Arr_2;
                            Position : Standard.Interfaces.Java.Int)
                            return Object_Arr;

   procedure Set_Object_Arr (Arr      : access Typ_Arr_2;
                             Position : Standard.Interfaces.Java.Int;
                             Value    : Object_Arr);

   ------------------------------
   -- Constructor Declarations --
   ------------------------------

   function New_Object  return Object;

   -------------------------
   -- Method Declarations --
   -------------------------
--  protected

   function Clone (This : access Typ)
                   return java.lang.Object.Object;

   function Equals (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Boolean;
--  protected

   procedure Finalize (This : access Typ);
--  final

   function GetClass (This : access Typ)
                      return java.lang.Object.Object;

   function HashCode (This : access Typ)
                      return Standard.Interfaces.Java.Int;
--  final

   procedure Notify (This : access Typ);
--  final

   procedure NotifyAll (This : access Typ);

   function ToString (This : access Typ)
                      return java.lang.Object.Object;
--  final

   procedure Wait (This : access Typ);
--  final

   procedure Wait (This : access Typ;
                   P1_Long : Standard.Interfaces.Java.Long);
--  final

   procedure Wait (This : access Typ;
                   P1_Long : Standard.Interfaces.Java.Long;
                   P2_Int : Standard.Interfaces.Java.Int);
private

   type Typ is new JNI_Data with null record;

   --  Root_Array type
   type Root_Array is abstract new Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Lang.Object;
limited with Java_Arrays;
with Interfaces.Java;
with Java.Lang.Object;

package Java.Lang.String is

   -----------------------
   -- Type Declarations --
   -----------------------

   --  final class
   type Typ is abstract new Java.Lang.Object.Typ with private;
   type String is access all Typ'Class;

   ------------------------
   -- Array Declarations --
   ------------------------

   --  Unidimensional array
   type Typ_Arr is abstract new Java.Lang.Object.Root_Array with private;
   type String_Arr is access all Typ_Arr'Class;

   function New_String_Arr (Length        : Standard.Interfaces.Java.Int;
                            Default_Value : String := null)
                            return String_Arr;

   function Get_String (Arr      : access Typ_Arr;
                        Position : Standard.Interfaces.Java.Int)
                        return String;

   procedure Set_String (Arr      : access Typ_Arr;
                         Position : Standard.Interfaces.Java.Int;
                         Value    : String);

   --  2 dimensional array
   type Typ_Arr_2 is abstract new Java.Lang.Object.Root_Array with private;
   type String_Arr_2 is access all Typ_Arr_2'Class;

   function New_String_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                              Default_Value : String_Arr := null)
                              return String_Arr_2;

   function Get_String_Arr (Arr      : access Typ_Arr_2;
                            Position : Standard.Interfaces.Java.Int)
                            return String_Arr;

   procedure Set_String_Arr (Arr      : access Typ_Arr_2;
                             Position : Standard.Interfaces.Java.Int;
                             Value    : String_Arr);

   ---------------------
   -- Field Accessors --
   ---------------------

   --  function Get_CASE_INSENSITIVE_ORDER return java.lang.Object.Object;

   ------------------------------
   -- Constructor Declarations --
   ------------------------------

   function New_String  return String;

   function New_String (P1_String : access Java.Lang.String.Typ'Class)
                        return String;

   function New_String (P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class)
                        return String;

   function New_String (P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int)
                        return String;

   function New_String (P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int)
                        return String;

   function New_String (P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int;
                        P4_Int : Standard.Interfaces.Java.Int)
                        return String;

   function New_String (P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int;
                        P4_String : access Java.Lang.String.Typ'Class)
                        return String;

   function New_String (P1_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                        P2_String : access Java.Lang.String.Typ'Class)
                        return String;

   function New_String (P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class)
                        return String;

   function New_String (P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int;
                        P3_Int : Standard.Interfaces.Java.Int)
                        return String;

   -------------------------
   -- Method Declarations --
   -------------------------

   function CharAt (This : access Typ;
                    P1_Int : Standard.Interfaces.Java.Int)
                    return Standard.Interfaces.Java.Char;

   function CompareTo (This : access Typ;
                       P1_Object : access Java.Lang.Object.Typ'Class)
                       return Standard.Interfaces.Java.Int;

   function CompareTo (This : access Typ;
                       P1_String : access Java.Lang.String.Typ'Class)
                       return Standard.Interfaces.Java.Int;

   function CompareToIgnoreCase (This : access Typ;
                                 P1_String : access Java.Lang.String.Typ'Class)
                                 return Standard.Interfaces.Java.Int;

   function Concat (This : access Typ;
                    P1_String : access Java.Lang.String.Typ'Class)
                    return java.lang.Object.Object;

   function CopyValueOf (P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class)
                         return java.lang.Object.Object;

   function CopyValueOf (P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class;
                         P2_Int : Standard.Interfaces.Java.Int;
                         P3_Int : Standard.Interfaces.Java.Int)
                         return java.lang.Object.Object;

   function EndsWith (This : access Typ;
                      P1_String : access Java.Lang.String.Typ'Class)
                      return Standard.Interfaces.Java.Boolean;

   function Equals (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Boolean;

   function EqualsIgnoreCase (This : access Typ;
                              P1_String : access Java.Lang.String.Typ'Class)
                              return Standard.Interfaces.Java.Boolean;

   function GetBytes (This : access Typ)
                      return java.lang.Object.Object;

   procedure GetBytes (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Byte_Arr : access Java_Arrays.Byte_Arr_Typ'Class;
                       P4_Int : Standard.Interfaces.Java.Int);

   function GetBytes (This : access Typ;
                      P1_String : access Java.Lang.String.Typ'Class)
                      return java.lang.Object.Object;

   procedure GetChars (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class;
                       P4_Int : Standard.Interfaces.Java.Int);

   function HashCode (This : access Typ)
                      return Standard.Interfaces.Java.Int;

   function IndexOf (This : access Typ;
                     P1_Int : Standard.Interfaces.Java.Int)
                     return Standard.Interfaces.Java.Int;

   function IndexOf (This : access Typ;
                     P1_Int : Standard.Interfaces.Java.Int;
                     P2_Int : Standard.Interfaces.Java.Int)
                     return Standard.Interfaces.Java.Int;

   function IndexOf (This : access Typ;
                     P1_String : access Java.Lang.String.Typ'Class)
                     return Standard.Interfaces.Java.Int;

   function IndexOf (This : access Typ;
                     P1_String : access Java.Lang.String.Typ'Class;
                     P2_Int : Standard.Interfaces.Java.Int)
                     return Standard.Interfaces.Java.Int;

   function Intern (This : access Typ)
                    return java.lang.Object.Object;

   function LastIndexOf (This : access Typ;
                         P1_Int : Standard.Interfaces.Java.Int)
                         return Standard.Interfaces.Java.Int;

   function LastIndexOf (This : access Typ;
                         P1_Int : Standard.Interfaces.Java.Int;
                         P2_Int : Standard.Interfaces.Java.Int)
                         return Standard.Interfaces.Java.Int;

   function LastIndexOf (This : access Typ;
                         P1_String : access Java.Lang.String.Typ'Class)
                         return Standard.Interfaces.Java.Int;

   function LastIndexOf (This : access Typ;
                         P1_String : access Java.Lang.String.Typ'Class;
                         P2_Int : Standard.Interfaces.Java.Int)
                         return Standard.Interfaces.Java.Int;

   function Length (This : access Typ)
                    return Standard.Interfaces.Java.Int;

   function RegionMatches (This : access Typ;
                           P1_Int : Standard.Interfaces.Java.Int;
                           P2_String : access Java.Lang.String.Typ'Class;
                           P3_Int : Standard.Interfaces.Java.Int;
                           P4_Int : Standard.Interfaces.Java.Int)
                           return Standard.Interfaces.Java.Boolean;

   function RegionMatches (This : access Typ;
                           P1_Boolean : Standard.Interfaces.Java.Boolean;
                           P2_Int : Standard.Interfaces.Java.Int;
                           P3_String : access Java.Lang.String.Typ'Class;
                           P4_Int : Standard.Interfaces.Java.Int;
                           P5_Int : Standard.Interfaces.Java.Int)
                           return Standard.Interfaces.Java.Boolean;

   function Replace (This : access Typ;
                     P1_Char : Standard.Interfaces.Java.Char;
                     P2_Char : Standard.Interfaces.Java.Char)
                     return java.lang.Object.Object;

   function StartsWith (This : access Typ;
                        P1_String : access Java.Lang.String.Typ'Class)
                        return Standard.Interfaces.Java.Boolean;

   function StartsWith (This : access Typ;
                        P1_String : access Java.Lang.String.Typ'Class;
                        P2_Int : Standard.Interfaces.Java.Int)
                        return Standard.Interfaces.Java.Boolean;

   function Substring (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int)
                       return java.lang.Object.Object;

   function Substring (This : access Typ;
                       P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int)
                       return java.lang.Object.Object;

   function ToCharArray (This : access Typ)
                         return java.lang.Object.Object;

   function ToLowerCase (This : access Typ)
                         return java.lang.Object.Object;

   function ToString (This : access Typ)
                      return java.lang.Object.Object;

   function ToUpperCase (This : access Typ)
                         return java.lang.Object.Object;

   function Trim (This : access Typ)
                  return java.lang.Object.Object;

   function ValueOf (P1_Char : Standard.Interfaces.Java.Char)
                     return java.lang.Object.Object;

   function ValueOf (P1_Double : Standard.Interfaces.Java.Double)
                     return java.lang.Object.Object;

   function ValueOf (P1_Float : Standard.Interfaces.Java.Float)
                     return java.lang.Object.Object;

   function ValueOf (P1_Int : Standard.Interfaces.Java.Int)
                     return java.lang.Object.Object;

   function ValueOf (P1_Long : Standard.Interfaces.Java.Long)
                     return java.lang.Object.Object;

   function ValueOf (P1_Object : access Java.Lang.Object.Typ'Class)
                     return java.lang.Object.Object;

   function ValueOf (P1_Boolean : Standard.Interfaces.Java.Boolean)
                     return java.lang.Object.Object;

   function ValueOf (P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class)
                     return java.lang.Object.Object;

   function ValueOf (P1_Char_Arr : access Java_Arrays.Char_Arr_Typ'Class;
                     P2_Int : Standard.Interfaces.Java.Int;
                     P3_Int : Standard.Interfaces.Java.Int)
                     return java.lang.Object.Object;
private

   type Typ is new Java.Lang.Object.Typ with null record;

   --  Unidimensional array
   type Typ_Arr is new Java.Lang.Object.Root_Array with null record;

   --  2 dimensional array
   type Typ_Arr_2 is new Java.Lang.Object.Root_Array with null record;

end Java.Lang.String;
package Java.Lang is
end Java.Lang;
package Java is
end Java;
with java.lang.Object;

package Java_Arrays is

   ---------------------------
   -- Primitive array types --
   ---------------------------

   type Boolean_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with private;
   type Boolean_Arr is access all Boolean_Arr_Typ'Class;

   type Char_Arr_Typ is abstract new Java.Lang.Object.Root_Array with private;
   type Char_Arr is access all Char_Arr_Typ'Class;

   type Byte_Arr_Typ is abstract new Java.Lang.Object.Root_Array with private;
   type Byte_Arr is access all Byte_Arr_Typ'Class;

   type Short_Arr_Typ is abstract new Java.Lang.Object.Root_Array with private;
   type Short_Arr is access all Short_Arr_Typ'Class;

   type Int_Arr_Typ is abstract new Java.Lang.Object.Root_Array with private;
   type Int_Arr is access all Int_Arr_Typ'Class;

   type Long_Arr_Typ is abstract new Java.Lang.Object.Root_Array with private;
   type Long_Arr is access all Long_Arr_Typ'Class;

   type Float_Arr_Typ is abstract new Java.Lang.Object.Root_Array with private;
   type Float_Arr is access all Float_Arr_Typ'Class;

   type Double_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with private;
   type Double_Arr is access all Double_Arr_Typ'Class;

   type Boolean_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with private;
   type Boolean_Arr2 is access all Boolean_Arr_Typ_2'Class;

   type Char_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Char_Arr2 is access all Char_Arr_Typ_2'Class;

   type Byte_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with private;
   type Byte_Arr2 is access all Byte_Arr_Typ_2'Class;

   type Short_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Short_Arr2 is access all Short_Arr_Typ_2'Class;

   type Int_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Int_Arr2 is access all Int_Arr_Typ_2'Class;

   type Long_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array with private;
   type Long_Arr2 is access all Long_Arr_Typ_2'Class;

   type Float_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with private;
   type Float_Arr2 is access all Float_Arr_Typ_2'Class;

   type Double_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with private;
   type Double_Arr2 is access all Double_Arr_Typ_2'Class;

private
   type Boolean_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Char_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Byte_Arr_Typ is new Java.Lang.Object.Root_Array
     with null record;

   type Short_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Int_Arr_Typ is new Java.Lang.Object.Root_Array
     with null record;

   type Long_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Float_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Double_Arr_Typ is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Boolean_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Char_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Byte_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Short_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Int_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Long_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Float_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

   type Double_Arr_Typ_2 is abstract new Java.Lang.Object.Root_Array
     with null record;

end Java_Arrays;

with Interfaces.C; use Interfaces.C;

package JNI.Operations is

   --------------------
   -- JNI Operations --
   --------------------

   function Get_Env (VM      : Java_VM;
                     Penv    : access JNI_Env;
                     Version : J_Int := JNI_Version_1_2)
                     return J_Int;
   pragma Import (C, Get_Env, "Get_Env");

   function JNI_Create_Java_VM (Pvm     : access Java_VM;
                                Penv    : access JNI_Env;
                                Vm_Args : Java_VM_Init_Args)
                                return J_Int;
   pragma Import (C, JNI_Create_Java_VM, "JNI_CreateJavaVM");

   function Get_Method_ID (Env   : JNI_Env;
                           Class : J_Class;
                           Name  : char_array;
                           Sig   : char_array)
                           return J_Method_ID;
   pragma Import (C, Get_Method_ID, "Get_Method_ID");

   function Get_Method_ID (Env   : JNI_Env;
                           Class : J_Class;
                           Name  : String;
                           Sig   : String)
                           return J_Method_ID;

   function Find_Class (Env  : JNI_Env;
                        Name : char_array)
                        return J_Class;
   pragma Import (C, Find_Class, "Find_Class");

   function Find_Class (Env  : JNI_Env;
                        Name : String)
                        return J_Class;


   function New_Object_A (Env       : JNI_Env;
                          Class     : J_Class;
                          Method_ID : J_Method_ID;
                          Args      : J_Value_Array)
                          return J_Object;
   pragma Import (C, New_Object_A, "New_Object_A");


   function New_Object_Array (Env   : JNI_Env;
                              Len   : J_Size;
                              Class : J_Class;
                              Init  : J_Object)
                              return J_Object_Array;
   pragma Import (C, New_Object_Array, "New_Object_Array");

   function Get_Array_Length (Env : JNI_Env;
                              Arr : J_Array)
                              return J_Size;
   pragma Import (C, Get_Array_Length, "Get_Array_Length");

   function Get_Object_Array_Element (Env   : JNI_Env;
                                      Arr   : J_Array;
                                      Index : J_Size)
                                      return J_Object;
   pragma Import (C, Get_Object_Array_Element, "Get_Object_Array_Element");

   procedure Set_Object_Array_Element (Env   : JNI_Env;
                                       Arr   : J_Array;
                                       Index : J_Size;
                                       Val   : J_Object);
   pragma Import (C, Set_Object_Array_Element, "Set_Object_Array_Element");

   function Call_Boolean_Method (Env       : JNI_Env;
                                 Obj       : J_Object;
                                 Method_ID : J_Method_ID;
                                 Args      : J_Value_Array)
                                 return Boolean;
   pragma Import (C, Call_Boolean_Method, "Call_Boolean_Method_A");

end JNI.Operations;
with Interfaces.Java; use Interfaces.Java;
with System;

package JNI is

   type JNI_Env is new System.Address;
   JNI_Null_Env : JNI_Env := JNI_Env (System.Null_Address);

   ---------------------
   -- Primitive types --
   ---------------------

   type    J_Boolean is mod 2**8;
   subtype J_Byte    is Interfaces.Java.Byte;
   subtype J_Char    is Interfaces.Java.Char;
   subtype J_Short   is Interfaces.Java.Short;
   subtype J_Int     is Interfaces.Java.Int;
   subtype J_Long    is Interfaces.Java.Long;
   subtype J_Float   is Interfaces.Java.Float;
   subtype J_Double  is Interfaces.Java.Double;

   subtype J_Size is J_Int;

   ---------------------
   -- Reference types --
   ---------------------

   type J_Object is new System.Address;
   J_Null_Object : constant J_Object := J_Object (System.Null_Address);

   type J_Class is new J_Object;
   J_Null_Class : constant J_Class := J_Class (J_Null_Object);

   type J_String    is new J_Object;
   type J_Array     is new J_Object;
   J_Null_Array : constant J_Array := J_Array (J_Null_Object);
   type J_Throwable is new J_Object;

   type J_Object_Array  is new J_Array;
   J_Null_Object_Array : constant J_Object_Array
     := J_Object_Array (System.Null_Address);

   type J_Boolean_Array is new J_Array;
   type J_Byte_Array    is new J_Array;
   type J_Char_Array    is new J_Array;
   type J_Short_Array   is new J_Array;
   type J_Int_Array     is new J_Array;
   type J_Long_Array    is new J_Array;
   type J_Float_Array   is new J_Array;
   type J_Double_Array  is new J_Array;

   -------------------------
   -- J_Boolean constants --
   -------------------------

   JNI_False : J_Boolean := 0;
   JNI_True  : J_Boolean := 1;

   ----------------------------
   -- Possible return values --
   ----------------------------

   JNI_OK        : J_Int :=  0;
   JNI_Err       : J_Int := -1;
   JNI_Edetached : J_Int := -2;
   JNI_Eversion  : J_Int := -3;
   JNI_Enomem    : J_Int := -4;
   JNI_Eexist    : J_Int := -5;
   JNI_Einval    : J_Int := -6;

   ----------------
   -- Union type --
   ----------------

   type JNI_Type is (Jboolean, Jbyte, Jchar, Jshort, Jint,
                     Jlong, Jfloat, Jdouble, Jobject);

   type J_Value (T : JNI_Type := Jobject) is
      record
         case T is
            when Jboolean =>
               Z : J_Boolean;
            when Jbyte =>
               B : J_Byte;
            when Jchar =>
               C : J_Char;
            when Jshort =>
               S : J_Short;
            when Jint =>
               I : J_Int;
            when Jlong =>
               J : J_Long;
            when Jfloat =>
               F : J_Float;
            when Jdouble =>
               D : J_Double;
            when Jobject  =>
               L : J_Object;
         end case;
      end record;
   pragma Unchecked_Union (J_Value);

   type J_Value_Array is array (Positive range <>) of J_Value;

   --------------------------
   -- Field and method IDs --
   --------------------------

   type J_Field_ID is new System.Address;
   type J_Method_ID is new System.Address;
   J_Null_Method_ID : J_Method_ID := J_Method_ID (System.Null_Address);

   --------------------
   -- Version number --
   --------------------

   JNI_Version_1_1 : J_Int := 16#0001_0001#;
   JNI_Version_1_2 : J_Int := 16#0001_0002#;
   JNI_Version_1_4 : J_Int := 16#0001_0004#;

   ------------
   -- JavaVM --
   ------------

   type Java_VM_Init_Args is record
      Version             : J_Int := JNI_Version_1_2;
      N_Options           : J_Int := 0;
      Options             : System.Address := System.Null_Address;
      Ignore_Unrecognized : J_Boolean := JNI_True;
   end record;

   type Java_VM is new System.Address;

   ----------------------
   --  Global variable --
   ----------------------

   Current_VM  : aliased Java_VM := Java_VM (System.Null_Address);
   Current_Env : aliased JNI_Env;

end JNI;
--  This package defines the root type of all the reference types generated
--  by the binding tool. The only type that directly inherits from this
--  root type is Java.Lang.Object.Typ . All other reference types inherit
--  from Java.Lang.Object.Typ or one of its descendant.

--  No operation is available in this package to ensure that the users of
--  this package do not modify the state of the objects by mistake.
--  The subprograms that allows to access the private part of JNI_Data can
--  be found in JNI_Object.Operations .

with JNI; use JNI;

package JNI_Object is

   type JNI_Data is abstract tagged limited private;
   --  The root type of all the reference types

   function  Get_J_Object (D : access JNI_Data) return J_Object;
   procedure Set_J_Object (D : access JNI_Data; O : J_Object);
   --  Get and Set the JNI J_Object type, ie the reference to the
   --  concrete Java object defined in the JVM


private
   type JNI_Data is tagged limited record
      --  all the fields useful to use JNI
      J_Obj : J_Object;
   end record;
end JNI_Object;
with Interfaces.Java; use Interfaces.Java;
with JNI; use JNI;
with JNI.Operations; use JNI.Operations;
with Java.Lang.String;
with Java_Arrays;

package body Java.Awt.Color.ColorSpace is

   -------------------
   -- JNI variables --
   -------------------

   JB_Class: J_Class := J_Null_Class;

   -------------------------
   -- Utility subprograms --
   -------------------------

   procedure Set_Class (Env : JNI_Env);

   ---------------
   -- Set_Class --
   ---------------

   procedure Set_Class (Env : JNI_Env) is
      C : constant Standard.String := "java/awt/color/ColorSpace";
   begin
      JB_Class := Find_Class (Env, C);
      if JB_Class = J_Null_Class then
         raise Program_Error;
      end if;
   end Set_Class;

   ------------------------
   -- New_ColorSpace_Arr --
   ------------------------

   function New_ColorSpace_Arr (Length        : Standard.Interfaces.Java.Int;
                                Default_Value : ColorSpace := null)
                                return ColorSpace_Arr
   is
   begin
      return null;
   end New_ColorSpace_Arr;
-----------------------------
   -- function Get_ColorSpace --
   -----------------------------

   function Get_ColorSpace (Arr      : access Typ_Arr;
                            Position : Standard.Interfaces.Java.Int)
                            return ColorSpace
   is
   begin
      return null;
   end Get_ColorSpace ;

   ------------------------------
   -- procedure Set_ColorSpace --
   ------------------------------

   procedure Set_ColorSpace (Arr      : access Typ_Arr;
                             Position : Standard.Interfaces.Java.Int;
                             Value    : ColorSpace)
   is
   begin
      return;
   end Set_ColorSpace ;

   
   --------------------------
   -- New_ColorSpace_Arr_2 --
   --------------------------

   function New_ColorSpace_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                                  Default_Value : ColorSpace_Arr := null)
                                  return ColorSpace_Arr_2
   is
   begin
      return null;
   end New_ColorSpace_Arr_2;
---------------------------------
   -- function Get_ColorSpace_Arr --
   ---------------------------------

   function Get_ColorSpace_Arr (Arr      : access Typ_Arr_2;
                                Position : Standard.Interfaces.Java.Int)
                                return ColorSpace_Arr
   is
   begin
      return null;
   end Get_ColorSpace_Arr ;

   ----------------------------------
   -- procedure Set_ColorSpace_Arr --
   ----------------------------------

   procedure Set_ColorSpace_Arr (Arr      : access Typ_Arr_2;
                                 Position : Standard.Interfaces.Java.Int;
                                 Value    : ColorSpace_Arr)
   is
   begin
      return;
   end Set_ColorSpace_Arr ;

   
   GetInstanceID : J_Method_ID := J_Null_Method_ID;

   -----------------
   -- GetInstance --
   -----------------

   function GetInstance (P1_Int : Standard.Interfaces.Java.Int)
                         return java.lang.Object.Object
   is
      
   begin
      return null;
      
   end GetInstance;

   GetNameID : J_Method_ID := J_Null_Method_ID;

   -------------
   -- GetName --
   -------------

   function GetName (This : access Typ;
                     P1_Int : Standard.Interfaces.Java.Int)
                     return java.lang.Object.Object
   is
      
   begin
      return null;
      
   end GetName;

   GetNumComponentsID : J_Method_ID := J_Null_Method_ID;

   ----------------------
   -- GetNumComponents --
   ----------------------

   function GetNumComponents (This : access Typ)
                              return Standard.Interfaces.Java.Int
   is
      
   begin
      return 0;
      
   end GetNumComponents;

   GetTypeID : J_Method_ID := J_Null_Method_ID;

   -------------
   -- GetType --
   -------------

   function GetType (This : access Typ)
                     return Standard.Interfaces.Java.Int
   is
      
   begin
      return 0;
      
   end GetType;

   IsCS_sRGBID : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- IsCS_sRGB --
   ---------------

   function IsCS_sRGB (This : access Typ)
                       return Standard.Interfaces.Java.Boolean
   is
      
   begin
      return False;
      
   end IsCS_sRGB;

end Java.Awt.Color.ColorSpace;
with Interfaces.Java; use Interfaces.Java;
with JNI; use JNI;
with JNI.Operations; use JNI.Operations;
with Java.Awt.Color.ColorSpace;
with Java.Awt.Image.ColorModel;
with Java.Awt.RenderingHints;
with Java.Lang.Object;
with Java.Lang.String;
with Java_Arrays;

package body Java.Awt.Color is

   -------------------
   -- JNI variables --
   -------------------

   JB_Class: J_Class := J_Null_Class;

   -------------------------
   -- Utility subprograms --
   -------------------------

   procedure Set_Class (Env : JNI_Env);

   ---------------
   -- Set_Class --
   ---------------

   procedure Set_Class (Env : JNI_Env) is
      C : constant Standard.String := "java/awt/Color";
   begin
      JB_Class := Find_Class (Env, C);
      if JB_Class = J_Null_Class then
         raise Program_Error;
      end if;
   end Set_Class;

   -------------------
   -- New_Color_Arr --
   -------------------

   function New_Color_Arr (Length        : Standard.Interfaces.Java.Int;
                           Default_Value : Color := null)
                           return Color_Arr
   is
   begin
      return null;
   end New_Color_Arr;
------------------------
   -- function Get_Color --
   ------------------------

   function Get_Color (Arr      : access Typ_Arr;
                       Position : Standard.Interfaces.Java.Int)
                       return Color
   is
   begin
      return null;
   end Get_Color ;

   -------------------------
   -- procedure Set_Color --
   -------------------------

   procedure Set_Color (Arr      : access Typ_Arr;
                        Position : Standard.Interfaces.Java.Int;
                        Value    : Color)
   is
   begin
      return;
   end Set_Color ;


   ---------------------
   -- New_Color_Arr_2 --
   ---------------------

   function New_Color_Arr_2 (Length        : Standard.Interfaces.Java.Int;
                             Default_Value : Color_Arr := null)
                             return Color_Arr_2
   is
   begin
      return null;
   end New_Color_Arr_2;
----------------------------
   -- function Get_Color_Arr --
   ----------------------------

   function Get_Color_Arr (Arr      : access Typ_Arr_2;
                           Position : Standard.Interfaces.Java.Int)
                           return Color_Arr
   is
   begin
      return null;
   end Get_Color_Arr ;

   -----------------------------
   -- procedure Set_Color_Arr --
   -----------------------------

   procedure Set_Color_Arr (Arr      : access Typ_Arr_2;
                            Position : Standard.Interfaces.Java.Int;
                            Value    : Color_Arr)
   is
   begin
      return;
   end Set_Color_Arr ;


   New_ColorID : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_Float : Standard.Interfaces.Java.Float;
                       P2_Float : Standard.Interfaces.Java.Float;
                       P3_Float : Standard.Interfaces.Java.Float)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID = J_Null_Method_Id then
         New_ColorID := Get_Method_ID (Env, JB_Class,
                                       Method_Name,
                                       "(FFF)V");
         if New_ColorID = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   New_ColorID_1 : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_Float : Standard.Interfaces.Java.Float;
                       P2_Float : Standard.Interfaces.Java.Float;
                       P3_Float : Standard.Interfaces.Java.Float;
                       P4_Float : Standard.Interfaces.Java.Float)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID_1 = J_Null_Method_Id then
         New_ColorID_1 := Get_Method_ID (Env, JB_Class,
                                         Method_Name,
                                         "(FFFF)V");
         if New_ColorID_1 = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   New_ColorID_2 : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_Int : Standard.Interfaces.Java.Int)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID_2 = J_Null_Method_Id then
         New_ColorID_2 := Get_Method_ID (Env, JB_Class,
                                         Method_Name,
                                         "(I)V");
         if New_ColorID_2 = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   New_ColorID_3 : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID_3 = J_Null_Method_Id then
         New_ColorID_3 := Get_Method_ID (Env, JB_Class,
                                         Method_Name,
                                         "(III)V");
         if New_ColorID_3 = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   New_ColorID_4 : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_Int : Standard.Interfaces.Java.Int;
                       P2_Int : Standard.Interfaces.Java.Int;
                       P3_Int : Standard.Interfaces.Java.Int;
                       P4_Int : Standard.Interfaces.Java.Int)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID_4 = J_Null_Method_Id then
         New_ColorID_4 := Get_Method_ID (Env, JB_Class,
                                         Method_Name,
                                         "(IIII)V");
         if New_ColorID_4 = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   New_ColorID_5 : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_Int : Standard.Interfaces.Java.Int;
                       P2_Boolean : Standard.Interfaces.Java.Boolean)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID_5 = J_Null_Method_Id then
         New_ColorID_5 := Get_Method_ID (Env, JB_Class,
                                         Method_Name,
                                         "(IZ)V");
         if New_ColorID_5 = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   New_ColorID_6 : J_Method_ID := J_Null_Method_ID;

   ---------------
   -- New_Color --
   ---------------

   function New_Color (P1_ColorSpace : access Java.Awt.Color.ColorSpace.Typ'Class;
                       P2_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class;
                       P3_Float : Standard.Interfaces.Java.Float)
                       return Color
   is
      Env    : aliased JNI_Env;
      I      : J_Int;
      Result : Color := new Typ;
      Method_Name : constant Standard.String := "<init>";

   begin

      --  Get the Environment
      I := Get_Env (VM   => Current_VM,
                    Penv => Env'Access);

      if I /= JNI_OK then
         raise Program_Error;
      end if;

      --  Check that the Class is set
      if JB_Class = J_Null_Class then
         Set_Class (Env);
      end if;

      --  Check that the method ID is set
      if New_ColorID_6 = J_Null_Method_Id then
         New_ColorID_6 := Get_Method_ID (Env, JB_Class,
                                         Method_Name,
                                         "(Ljava/awt/color/ColorSpace;[FF)V");
         if New_ColorID_6 = J_Null_Method_Id then
            raise Program_Error;
         end if;
      end if;

      --  Construct the parameter table
      return null;

   end New_Color;

   HSBtoRGBID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- HSBtoRGB --
   --------------

   function HSBtoRGB (P1_Float : Standard.Interfaces.Java.Float;
                      P2_Float : Standard.Interfaces.Java.Float;
                      P3_Float : Standard.Interfaces.Java.Float)
                      return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end HSBtoRGB;

   RGBtoHSBID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- RGBtoHSB --
   --------------

   function RGBtoHSB (P1_Int : Standard.Interfaces.Java.Int;
                      P2_Int : Standard.Interfaces.Java.Int;
                      P3_Int : Standard.Interfaces.Java.Int;
                      P4_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                      return java.lang.Object.Object
   is

   begin
      return null;

   end RGBtoHSB;

   BrighterID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- Brighter --
   --------------

   function Brighter (This : access Typ)
                      return java.lang.Object.Object
   is

   begin
      return null;

   end Brighter;

   ------------
   -- Darker --
   ------------

   function Darker (This : access Typ)
                    return java.lang.Object.Object
   is

   begin
      return null;

   end Darker;

   DecodeID : J_Method_ID := J_Null_Method_ID;

   ------------
   -- Decode --
   ------------

   function Decode (P1_String : access Java.Lang.String.Typ'Class)
                    return java.lang.Object.Object
   is

   begin
      return null;

   end Decode;

   EqualsID : J_Method_ID := J_Null_Method_ID;

   ------------
   -- Equals --
   ------------

   function Equals (This : access Typ;
                    P1_Object : access Java.Lang.Object.Typ'Class)
                    return Standard.Interfaces.Java.Boolean
   is

   begin
      return False;

   end Equals;

   GetAlphaID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- GetAlpha --
   --------------

   function GetAlpha (This : access Typ)
                      return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end GetAlpha;

   GetBlueID : J_Method_ID := J_Null_Method_ID;

   -------------
   -- GetBlue --
   -------------

   function GetBlue (This : access Typ)
                     return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end GetBlue;

   GetColorID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- GetColor --
   --------------

   function GetColor (P1_String : access Java.Lang.String.Typ'Class)
                      return java.lang.Object.Object
   is

   begin
      return null;

   end GetColor;

   GetColorID_1 : J_Method_ID := J_Null_Method_ID;

   --------------
   -- GetColor --
   --------------

   function GetColor (P1_String : access Java.Lang.String.Typ'Class;
                      P2_Int : Standard.Interfaces.Java.Int)
                      return java.lang.Object.Object
   is

   begin
      return null;

   end GetColor;

   GetColorID_2 : J_Method_ID := J_Null_Method_ID;

   --------------
   -- GetColor --
   --------------

   function GetColor (P1_String : access Java.Lang.String.Typ'Class;
                      P2_Color : access Java.Awt.Color.Typ'Class)
                      return java.lang.Object.Object
   is

   begin
      return null;

   end GetColor;

   GetColorComponentsID : J_Method_ID := J_Null_Method_ID;

   ------------------------
   -- GetColorComponents --
   ------------------------

   function GetColorComponents (This : access Typ;
                                P1_ColorSpace : access Java.Awt.Color.ColorSpace.Typ'Class;
                                P2_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                                return java.lang.Object.Object
   is

   begin
      return null;

   end GetColorComponents;

   GetColorComponentsID_1 : J_Method_ID := J_Null_Method_ID;

   ------------------------
   -- GetColorComponents --
   ------------------------

   function GetColorComponents (This : access Typ;
                                P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                                return java.lang.Object.Object
   is

   begin
      return null;

   end GetColorComponents;

   GetColorSpaceID : J_Method_ID := J_Null_Method_ID;

   -------------------
   -- GetColorSpace --
   -------------------

   function GetColorSpace (This : access Typ)
                           return java.lang.Object.Object
   is

   begin
      return null;

   end GetColorSpace;

   GetComponentsID : J_Method_ID := J_Null_Method_ID;

   -------------------
   -- GetComponents --
   -------------------

   function GetComponents (This : access Typ;
                           P1_ColorSpace : access Java.Awt.Color.ColorSpace.Typ'Class;
                           P2_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                           return java.lang.Object.Object
   is

   begin
      return null;

   end GetComponents;

   GetComponentsID_1 : J_Method_ID := J_Null_Method_ID;

   -------------------
   -- GetComponents --
   -------------------

   function GetComponents (This : access Typ;
                           P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                           return java.lang.Object.Object
   is

   begin
      return null;

   end GetComponents;

   GetGreenID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- GetGreen --
   --------------

   function GetGreen (This : access Typ)
                      return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end GetGreen;

   GetHSBColorID : J_Method_ID := J_Null_Method_ID;

   -----------------
   -- GetHSBColor --
   -----------------

   function GetHSBColor (P1_Float : Standard.Interfaces.Java.Float;
                         P2_Float : Standard.Interfaces.Java.Float;
                         P3_Float : Standard.Interfaces.Java.Float)
                         return java.lang.Object.Object
   is

   begin
      return null;

   end GetHSBColor;

   GetRGBID : J_Method_ID := J_Null_Method_ID;

   ------------
   -- GetRGB --
   ------------

   function GetRGB (This : access Typ)
                    return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end GetRGB;

   GetRGBColorComponentsID : J_Method_ID := J_Null_Method_ID;

   ---------------------------
   -- GetRGBColorComponents --
   ---------------------------

   function GetRGBColorComponents (This : access Typ;
                                   P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                                   return java.lang.Object.Object
   is

   begin
      return null;

   end GetRGBColorComponents;

   GetRGBComponentsID : J_Method_ID := J_Null_Method_ID;

   ----------------------
   -- GetRGBComponents --
   ----------------------

   function GetRGBComponents (This : access Typ;
                              P1_Float_Arr : access Java_Arrays.Float_Arr_Typ'Class)
                              return java.lang.Object.Object
   is

   begin
      return null;

   end GetRGBComponents;

   GetRedID : J_Method_ID := J_Null_Method_ID;

   ------------
   -- GetRed --
   ------------

   function GetRed (This : access Typ)
                    return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end GetRed;

   GetTransparencyID : J_Method_ID := J_Null_Method_ID;

   ---------------------
   -- GetTransparency --
   ---------------------

   function GetTransparency (This : access Typ)
                             return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end GetTransparency;

   HashCodeID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- HashCode --
   --------------

   function HashCode (This : access Typ)
                      return Standard.Interfaces.Java.Int
   is

   begin
      return 0;

   end HashCode;

   ToStringID : J_Method_ID := J_Null_Method_ID;

   --------------
   -- ToString --
   --------------

   function ToString (This : access Typ)
                      return java.lang.Object.Object
   is

   begin
      return null;

   end ToString;

end Java.Awt.Color;
package body JNI.Operations is

   -------------------
   -- Get_Method_Id --
   -------------------

   function Get_Method_ID (Env   : JNI_Env;
                           Class : J_Class;
                           Name  : String;
                           Sig   : String)
                           return J_Method_ID
   is
   begin
      return Get_Method_ID (Env,
                            Class,
                            To_C (Name, Append_Nul => True),
                            To_C (Sig,  Append_Nul => True));
   end Get_Method_ID;

   ----------------
   -- Find_Class --
   ----------------

   function Find_Class (Env  : JNI_Env;
                        Name : String)
                        return J_Class
   is
   begin
      return Find_Class (Env, To_C (Name));
   end Find_Class;

end JNI.Operations;
package body JNI_Object is

   ------------------
   -- Get_J_Object --
   ------------------

   function Get_J_Object (D : access JNI_Data) return J_Object
   is
   begin
      if D = null then
         return J_Null_Object;
      else
         return D.J_Obj;
      end if;
   end Get_J_Object;

   ------------------
   -- Set_J_Object --
   ------------------

   procedure Set_J_Object (D : access JNI_Data; O : J_Object)
   is
   begin
      D.J_Obj := O;
   end Set_J_Object;

end JNI_Object;
