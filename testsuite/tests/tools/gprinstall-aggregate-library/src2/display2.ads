with Displaylib;

package Display2 is
	function Hello return String;
   function Hello return String is ("Hello " & Displaylib.World
                                    & " from prj2 !");
end Display2;
