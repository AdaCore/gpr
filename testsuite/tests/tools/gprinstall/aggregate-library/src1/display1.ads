with Displaylib;

package Display1 is
	function Hello return String;
   function Hello return String is ("Hello " & Displaylib.World
                                    & " from prj1 !");
end Display1;
