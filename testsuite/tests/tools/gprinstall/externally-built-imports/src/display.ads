with Displaylib;

package Display is
	function Hello return String;
    function Hello return String is ("Hello " & Displaylib.World
                                     & " from prj !");
end Display;
