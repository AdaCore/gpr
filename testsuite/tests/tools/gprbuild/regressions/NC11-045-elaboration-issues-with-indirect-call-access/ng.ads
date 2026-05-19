

with Gen_G;
package Ng is
  function Image (Value : in Integer) return String;

  package Gen is new Gen_G (Image'Access);
end Ng;
