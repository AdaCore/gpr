#! /usr/bin/env python
# PURPOSE: Provides classes for handling of sets of numbers:
#
#     Interval: A continous set of numbers in a range from a
#              minimum to a maximum border.
# 
#     NumberSet: A non-continous set of numbers consisting of
#                described as a set of intervals.
#
# DATE: May 26, 2006
#
# (C) 2006-2014 Frank-Rene Schaefer
#
# ABSOLUTELY NO WARRANTY
################################################################################


# import quex.output.core.dictionary as languages
import quex.engine.misc.utf8 as utf8
from   quex.engine.misc.tools import r_enumerate, \
                                typed, \
                                flatten_list_of_lists

import sys
from   copy      import copy
from   itertools import izip, islice


class Interval(object):
    """Representing an interval with a minimum and a maximum border. Implements
    basic operations on intervals: union, intersection, and difference.
    """
    __slots__ = ('begin', 'end')

    def __init__(self, Begin=None, End=None):
        """NOTE: Begin = End signifies **empty** interval.

        Begin is None and End is None   => Empty Interval

        Begin == int and End is None    => Interval of size '1' (one number = Begin)
        
        Begin and End is not None           => Interval starting from 'Begin' and the last
                                           element is 'End-1'

        """

        # .begin = smallest integer that belogns to interval.
        # .end   = first integer > 'Begin' that does **not belong** to interval
        if Begin is None and End is None:
            # empty interval
            self.begin = 0
            self.end   = 0
        else:
            assert Begin is not None, "Begin can only be 'None', if End is also 'None'!"

            self.begin = Begin            
            if End is None:  
                if self.begin != sys.maxint: self.end = self.begin + 1
                else:                        self.end = self.begin
            else:    
                self.end = End
            
    def clone(self):
        return Interval(self.begin, self.end)

    def is_empty(self):
        return self.begin == self.end

    def is_all(self):
        return self.begin == -sys.maxint and self.end == sys.maxint   

    def is_equal(self, Other):
        return self.begin == Other.begin and self.end == Other.end
 
    def contains(self, Number):
        """True  => if Number in NumberSet
           False => else
        """
        if Number >= self.begin and Number < self.end: return True
        else:                                          return False

    def contains_only(self, Number):
        return self.begin == Number and self.end == Number + 1
        
    def check_overlap(self, Other):
        """Does interval overlap the Other?"""
        if self.begin  < Other.end and self.end > Other.begin: return True
        if Other.begin < self.end  and Other.end > self.begin: return True
        else:                                                  return False

    def check_touch(self, Other):
        """Does interval touch the Other?"""
        if self.begin  < Other.end and self.end > Other.begin:   return True
        if Other.begin < self.end  and Other.end > self.begin:   return True
        if self.begin == Other.begin or self.end == Other.begin: return True
        else:                                                    return False
    
    def union(self, Other):
        if self.check_overlap(Other):
            # overlap: return one single interval
            #          (the one that encloses both intervals)
            return [ Interval(min(self.begin, Other.begin),
                              max(self.end, Other.end)) ]
        else:
            # no overlap: two disjunct intervals
            result = []
            if not self.is_empty(): result.append(copy(self))
            if not Other.is_empty(): result.append(copy(Other))
            return result

    def intersection(self, Other):
        if self.check_overlap(Other):
            # overlap: return one single interval
            #          (the one that both have in common)
            return Interval(max(self.begin, Other.begin),
                            min(self.end, Other.end)) 
        else:
            # no overlap: empty interval (begin=end)
            return Interval()  # empty interval

    def difference(self, Other):
        """Difference self - Other."""
        if self.begin >= Other.begin:
            if self.end <= Other.end:
                # overlap: Other covers self
                # --------------[xxxxxxxxxxxxx]-------------------
                # ---------[yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy]-----
                #
                #          nothing remains - empty interval
                return []
            else:
                if self.begin >= Other.end:
                    # no intersection
                    return [ copy(self) ]
                else:
                    # overlap: Other covers lower part
                    # --------------[xxxxxxxxxxxxxxxxxxxx]------------
                    # ---------[yyyyyyyyyyyyyyyyyyyy]-----------------
                    return [ Interval(Other.end, self.end) ]
        else:            
            if self.end >= Other.end:
                # overlap: self covers Other
                # ---------[xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx]-----
                # --------------[yyyyyyyyyyyyy]-------------------
                #
                # result = two disjunct intervals
                result = []
                lower_part = Interval(self.begin, Other.begin)
                if not lower_part.is_empty(): result.append(lower_part)
                upper_part = Interval(Other.end, self.end)
                if not upper_part.is_empty(): result.append(upper_part)
                return result
            else:
                if self.end <= Other.begin:
                    # no intersection
                    return [ copy(self) ]
                else:
                    # overlap: Other upper part
                    # ---------[xxxxxxxxxxxxx]------------------------
                    # --------------[yyyyyyyyyyyyy]-------------------
                    #                
                    return [ Interval(self.begin, Other.begin) ]

    def transform(self, TrafoInfo):
        """RETURNS: verdict, list

           where 'verdict' indicates whether the interval has been completely
           transformed. 'list' is the resulting list of intervals.
        """
        transformed_n = 0
        result        = []
        end   = self.end
        begin = self.begin
        for i, entry  in enumerate(TrafoInfo): 
            source_begin, source_end, target_begin = entry
            if source_end > begin: break
        else: 
            return False, []

        L = len(TrafoInfo)
        while source_begin < end:
            current_begin = max(begin, source_begin)
            offset_begin  = current_begin - source_begin
            current_end   = min(end, source_end)
            offset_end    = current_end - source_begin

            result.append(Interval(target_begin + offset_begin, 
                                   target_begin + offset_end))

            transformed_n += (current_end - current_begin)

            begin = current_end
            i += 1
            if i == L: break
            source_begin, source_end, target_begin = TrafoInfo[i]

        verdict = transformed_n == (self.end - self.begin)
        return verdict, result

    def get_complement(self, Begin, End):
        if self.begin == self.end:
            # empty interval => whole range
            return [ Interval(Begin, End) ]
        else:
            result = []
            if self.begin != Begin: result.append(Interval(Begin, self.begin))
            if self.end   != End:   result.append(Interval(self.end, End))
            return result

    def size(self):
        return self.end - self.begin

    def __repr__(self):
        return self.get_string(Option="")

    def get_string(self, Option="", Delimiter=", "):
        assert Option in ("hex", "dec", "utf8", "")
        assert self.end >= self.begin
        if   Option == "hex":  
            __repr = lambda x:     "-oo" if x == - sys.maxint   \
                               else "oo" if x == sys.maxint - 1 \
                               else "%04X" % x
        elif Option == "utf8": 
            __repr = lambda x: utf8.unicode_to_pretty_utf8(x)
        else: 
            __repr = lambda x:     "-oo" if x == - sys.maxint   \
                               else "oo" if x == sys.maxint - 1 \
                               else "%s" % x
        
        if self.begin == self.end:       return "[]"
        elif self.end - self.begin == 1: return "[" + __repr(self.begin) + "]" 
        else:                            return "[" + __repr(self.begin) + Delimiter + __repr(self.end-1) + "]"

    def get_utf8_string(self):
        #_____________________________________________________________________________
        assert self.begin <= self.end
        
        if self.begin == self.end: 
            return "''"
        elif self.end - self.begin == 1: 
            return utf8.unicode_to_pretty_utf8(self.begin) 
        else:                          
            if   self.end == -sys.maxint: end_char = "-oo"
            elif self.end == sys.maxint:  end_char = "oo"
            else:                         end_char = utf8.unicode_to_pretty_utf8(self.end-1)
            return "[" + utf8.unicode_to_pretty_utf8(self.begin) + ", " + end_char + "]"

    def gnuplot_string(self, y_coordinate):
        if self.begin == self.end: return ""
        txt = ""
        txt += "%i %f\n" % (self.begin, y_coordinate)
        txt += "%i %f\n" % (self.end-1, y_coordinate)
        txt += "%i %f\n" % (self.end-1, float(y_coordinate) + 0.8)
        txt += "%i %f\n" % (self.begin, float(y_coordinate) + 0.8)
        txt += "%i %f\n" % (self.begin, y_coordinate)
        return txt

    def __eq__(self, Other):
        if Other is None: return False
        return self.begin == Other.begin and self.end == Other.end

    def __ne__(self, Other):
        if Other is None: return True
        return self.begin != Other.begin or self.end != Other.end

    def __lt__(self, Other):
        if   self.begin < Other.begin: return True
        elif self.begin > Other.begin: return False
        elif self.end   < Other.end:   return True
        elif self.end   > Other.end:   return False
        else:                          return False  # The 'equal' case

    def __le__(self, Other):
        if   self.begin < Other.begin: return True
        elif self.begin > Other.begin: return False
        elif self.end   < Other.end:   return True
        elif self.end   > Other.end:   return False
        else:                          return True   # The 'equal' case

    def __gt__(self, Other):
        if   self.begin > Other.begin: return True
        elif self.begin < Other.begin: return False
        elif self.end   > Other.end:   return True
        elif self.end   < Other.end:   return False
        else:                          return False  # The 'equal' case

    def __ge__(self, Other):
        if   self.begin > Other.begin: return True
        elif self.begin < Other.begin: return False
        elif self.end   > Other.end:   return True
        elif self.end   < Other.end:   return False
        else:                          return True   # The 'equal' case

    def __cmp__(self, Other):
        if   self.begin < Other.begin: return -1
        elif self.begin > Other.begin: return  1
        elif self.end   < Other.end:   return -1
        elif self.end   > Other.end:   return  1
        return 0

class NumberSet(object):
    """Represents an arbitrary set of numbers. The set is described
       in terms of intervals, i.e. objects of class 'Interval'. This
       class also provides basic operations such as union, intersection,
       and difference.
    """

    __slots__ = ('__intervals')

    def __init__(self, Arg = None, ArgumentIsYoursF=False):
        """Arg = list     ==> list of initial intervals
           Arg = Interval ==> initial interval
           Arg = integer  ==> interval consisting of one number
           """
        arg_type = Arg.__class__
        
        if  arg_type == list:
            if ArgumentIsYoursF:
                self.__intervals = Arg
            else:
                self.__intervals = []
                # use 'add_interval' to ensure consistency, i.e. touches, overlaps, etc.
                for interval in Arg:
                    self.add_interval(copy(interval))

        elif  arg_type == Interval:
            if ArgumentIsYoursF: self.__intervals = [ Arg ] 
            else:                self.__intervals = [ copy(Arg) ]

        elif arg_type == NumberSet:
            if ArgumentIsYoursF:  self.__intervals = Arg.__intervals
            else:                 self.__intervals = Arg.__clone_intervals()

        elif arg_type == int:
            self.__intervals = [ Interval(Arg) ]

        elif Arg is None:
            self.__intervals = []

        else:
            # assert: arg_type in [Interval, NumberSet, int, list] or Arg is None
            assert False, "#Arg: '%s'" % Arg 

    @staticmethod
    def from_range(Begin, End):
        result = NumberSet()
        result.add_interval(Interval(Begin, End))
        return result

    @staticmethod
    def from_union_of_iterable(Iterable):
        result = NumberSet()
        for x in Iterable:
            result.unite_with(x)
        return result

    def __clone_intervals(self):
        return [ Interval(x.begin, x.end) for x in self.__intervals ]

    def __bisect(self, Value):
        if len(self.__intervals) == 0:
            return None
        lower = 0
        upper = len(self.__intervals)
        while upper - lower > 1:
            i = (upper + lower) >> 1
            if   self.__intervals[i].begin >  Value: upper = i
            elif self.__intervals[i].end   <= Value: lower = i
            else:                                    return i

        if     Value >= self.__intervals[lower].begin \
           and Value <  self.__intervals[lower].end:
            return lower

        return None

    def clone(self):
        return NumberSet([Interval(x.begin, x.end) for x in self.__intervals], ArgumentIsYoursF=True)

    def quick_append_interval(self, Other, SortF=True):
        """This function assumes that there are no intersections with other intervals.
           Use this function with caution. It is much faster than the 'union' function
           or the function 'add_interval'.
        """
        assert Other.__class__.__name__ == "Interval"

        self.__intervals.append(Other)

    def quick_append_value(self, Value):
        x = self.__intervals
        if len(x) != 0  and x[-1].end == Value: x[-1].end = Value + 1
        else:                                   x.append(Interval(Value))

    def add_interval(self, X):
        """Adds an interval and ensures that no overlap with existing
        intervals occurs. Note: the 'touch' test is faster here, because
        only one interval is checked against.!
        
        IDEA: Return the position of the last insertion, so that this
              function could take it as a hint!
        """
        if X.begin == X.end: 
            return
        
        # (1) determine if begin overlaps with the new interval
        L = len(self.__intervals)
        if L == 0 or X.begin > self.__intervals[-1].end:
            self.__intervals.append(Interval(X.begin, X.end))
            return

        for i, y in enumerate(self.__intervals):
            # possible cases:
            #  (1) [=== X ===]         [=== y ===]             
            #  
            #  (2) [=== X ============][=== y ===]
            #  
            #  (3) [=== X ==================]
            #                          [=== y ===]
            #  
            #  (4) [=== X =======================]
            #                          [=== y ===]
            #  
            #  (5) [=== X =============================================]
            #                          [=== y ===]
            #  
            #  (6)                     [=== X =========================]
            #                          [=== y ===]
            #  
            #  (7)                     [=== y ===][=== X ==============]
            #  
            #  (8)                     [=== y ===]           [=== X ===]
            #  
            if X.begin > y.end: 
                continue                              # filter (8)
            elif X.end < y.begin: 
                self.__intervals.insert(i, Interval(X.begin, X.end)) 
                return                                # filter (1)         --> insert before 'y'
            else:
                begin = min(X.begin, y.begin)
                end   = max(X.end,   y.end)
                break

        # 'y' intersects with 'X', y is going to 'eat'
        # delete all intervals which are covered by 'X'
        if i + 1 < L:
            for k in xrange(i+1, L):
                z = self.__intervals[k]
                if X.end < z.begin: break
                end = max(end, z.end)
            else:
                k = L 
            # 'X' does touch 'z', i.e. '__intervals[k]'
            del self.__intervals[i+1:k] # delete the overlapped intervals

        y.begin = begin  # let 'y' take the new values
        y.end   = end

    def contains(self, Number):
        """True  => if Number in NumberSet
           False => else
        """
        return self.__bisect(Number) is not None

    def contains_only(self, Number):
        if   len(self.__intervals) != 1: return False
        x = self.__intervals[0]
        if x.end - x.begin != 1:         return False
        return x.begin == Number

    def has_size_one(self):
        if len(self.__intervals) != 1: return False
        return (self.__intervals[0].end - self.__intervals[0].begin) == 1

    def minimum(self):
        if len(self.__intervals) == 0: return sys.maxint   # i.e. an absurd value
        else:                          return self.__intervals[0].begin

    def supremum(self):
        if len(self.__intervals) == 0: return - sys.maxint # i.e. an absurd value
        else:                          return self.__intervals[-1].end

    def is_empty(self):
        if len(self.__intervals) == 0: return True
        for interval in self.__intervals:
            if interval.is_empty() == False: return False
        return True
        
    def is_all(self):
        """Returns True if this NumberSet covers all numbers, False if not.
           
           Note: All intervals should have been added using the function 'add_interval'
                 Thus no overlapping intervals shall exists. If the set covers all numbers,
                 then there can only be one interval that 'is_all()'
        """
        if len(self.__intervals) != 1: return False
        return self.__intervals[0].is_all()
            
    def is_equal(self, Other):
        """Assume: All intervals are sorted and adjacent intervals are combined.
        """
        if len(self.__intervals) != len(Other.__intervals): 
            return False
        for interval, other in izip(self.__intervals, Other.__intervals):
            if   interval.begin != other.begin: return False
            elif interval.end   != other.end:   return False
        return True

    def is_superset(self, Other):
        """True  -- if self covers Other
           False -- if not
        """
        self_intervals  = self.__intervals
        other_intervals = Other.__intervals

        # Last index of each set
        Li_self  = len(self_intervals) - 1
        Li_other = len(other_intervals) - 1

        if Li_self == -1:
            if Li_other == -1: return True
            else:              return False
        elif Li_other == -1:   return True

        # Step in a 'tolerant' manner, i.e. combine adjacent intervals
        # (Even though, adjacent intervals should have been combined before)
        i = 0
        k = 0
        other_begin = other_intervals[0].begin

        while 1 + 1 == 2:
            # Walk with 'i' in 'self' until we find something that intersects with the 
            # other interval 'k'.
            while self_intervals[i].end < other_begin:
                if i == Li_self: return False
                i += 1

            if self_intervals[i].begin > other_begin:
                return False

            elif self_intervals[i].end >= other_intervals[k].end:
                # Other interval is completely covered, next please.
                if k == Li_other: return True
                k += 1
                other_begin = other_intervals[k].begin
            else:
                # The part in 'other' from self_intervals[i].end to other_intervals[k].end
                # remains to be covered by the next intervals of 'self'.
                other_begin = self_intervals[i].end
                if i == Li_self: return False
                i += 1

    def interval_number(self):
        """This value gives some information about the 'complexity' of the number set."""
        return len(self.__intervals)

    def unite_with(self, Other):
        Other_type = Other.__class__
        # assert Other_type == Interval or Other_type == NumberSet, \
        #       "Error, argument of type %s" % Other.__class__.__name__

        if Other_type == Interval:  
            self.add_interval(Other)
            return

        elif len(Other.__intervals) == 0:
            return

        elif len(self.__intervals) == 0:
            self.__intervals = [
                copy(other) for other in Other.__intervals
            ]
            return

        elif Other.__intervals[0].begin > self.__intervals[-1].end:
            self.__intervals.extend(
                copy(other) for other in Other.__intervals
            )
            return

        elif Other.__intervals[-1].end < self.__intervals[0].begin:
            new_intervals = [ copy(other) for other in Other.__intervals ]
            new_intervals.extend(self.__intervals)
            self.__intervals = new_intervals
            return

        # simply add all intervals to one single set
        for interval in Other.__intervals:
            self.add_interval(interval)
            # IDEA: hint = self.add_interval(interval, hint)

    def union(self, Other):
        assert Other.__class__ in (Interval, NumberSet), \
               "Error, argument of type %s" % Other.__class__.__name__

        clone = self.clone() 
        clone.unite_with(Other)

        return clone            

    def has_intersection(self, Other):
        assert isinstance(Other, (Interval, NumberSet))
        if   len(self.__intervals) == 0:                                   return False
        elif Other.__class__ == NumberSet and len(Other.__intervals) == 0: return False

        self_begin = self.__intervals[0].begin
        self_end   = self.__intervals[-1].end
        if isinstance(Other, Interval): 
            assert False, "Try to delete this code, 'Other' shall be NumberSet!"
            if Other.end   < self_begin: return False
            if Other.begin > self_end:   return False

            for y in self.__intervals:
                # PASTE: Implement Interval::overlap() for performance reasons.
                if   Other.begin >= y.end:   continue
                elif Other.end   <= y.begin: break
                # x.end > y.begin  (lacks condition: x.begin < y.end)
                # y.end > x.begin  (lacks condition: y.begin < x.end)
                elif Other.begin < y.end or y.begin < Other.end: return True

            return False

        Other_begin = Other.__intervals[0].begin
        Other_end   = Other.__intervals[-1].end
        if   Other_end   < self_begin: return False
        elif Other_begin > self_end:   return False

        for x in Other.__intervals:
            if   x.end   < self_begin: continue
            elif x.begin > self_end:   break
            for y in self.__intervals:
                # PASTE: Implement Interval::overlap() for performance reasons.
                if   x.begin >= y.end:   continue
                elif x.end   <= y.begin: break
                # x.end > y.begin  (lacks condition: x.begin < y.end)
                # y.end > x.begin  (lacks condition: y.begin < x.end)
                elif x.begin < y.end or y.begin < x.end: return True
        return False

    def intersect_with(self, Other):
        assert Other.__class__ == Interval or Other.__class__ == NumberSet

        if Other.__class__ == Interval: Other_intervals = [ Other ]
        else:                           Other_intervals = Other.__intervals
        
        if len(Other_intervals) == 0 or len(self.__intervals) == 0:     
            self.__intervals = []
            return 

        # For each interval to remain, it needs at least have an intersection
        # with one of the other intervals. If such an intersection is found the
        # interval of concern can be pruned appropriately.
        Other_min_i = 0
        result = []
        for x in self.__intervals:
            for i, y in enumerate(islice(Other_intervals, Other_min_i, None), Other_min_i):
                if x.end    < y.begin: continue # not yet reached
                Other_min_i = i
                if x.begin >= y.end:   continue # overstepped 

                # Here:                    x.end   
                #                |---------------------------->
                #                          x.begin
                #            <-------------------------|
                #         
                #         -------|---------------------|--------
                #             y.begin                y.end
                begin = max(x.begin, y.begin)
                end   = min(x.end, y.end)
                if begin != end: result.append(Interval(begin, end))

        self.__intervals = result

    def intersection(self, Other):
        assert Other.__class__ == Interval or Other.__class__ == NumberSet

        # NOTE: If, for any reason this function does not rely on intersect_with(), then
        #       the function intersect_with() is no longer under unit test!
        result = self.clone()
        result.intersect_with(Other)
        return result

    def subtract(self, Other):
        Other_type = Other.__class__
        assert Other_type == Interval or Other_type == NumberSet, \
               "Error, argument of type %s" % Other.__class__.__name__

        if Other_type == Interval:  
            self.cut_interval(Other)
            return

        if len(self.__intervals) == 0:
            return

        Begin = self.__intervals[0].begin
        End   = self.__intervals[-1].end
        for interval in Other.__intervals:
            if interval.end   <= Begin: continue
            if interval.begin >= End:   break
            self.cut_interval(interval)

    def cut_lesser(self, Begin):
        """Cuts out any range that is below 'Begin'."""
        for i, x in enumerate(self.__intervals):
            if x.end > Begin: break
        else:
            del self.__intervals[:]
            return

        # HERE: '__intervals[i]' is the first interval that 
        #       intersects with 'Begin-oo'
        if x.begin < Begin: x.begin = Begin

        if i == 0: return

        # Delete all intervals that where skipped.
        del self.__intervals[:i]

    def cut_greater_or_equal(self, End):
        """Cuts out any range that is above or equal 'End'."""
        for i, x in r_enumerate(self.__intervals):
            if x.begin < End: break
        else:
            del self.__intervals[:]
            return

        # HERE: '__intervals[i]' is the first interval that 
        #       intersects with 'End-oo'
        if x.end > End: x.end = End

        if i == len(self.__intervals): return

        # Delete all intervals that where skipped.
        del self.__intervals[i+1:]

    def mask(self, Begin, End):
        """Begin = first element in range to include.
           End   = first element after the range to include.
        """
        self.cut_lesser(Begin)
        self.cut_greater_or_equal(End)

    def covers_range(self, Begin, End):
        """Begin = first element in range to include.
           End   = first element after the range to include.

                begin                 end
                  |                    |
           [ ][ ][x][x][x][x][x][x][x][ ][ ][ ][ ][ ]
                    '-------------'
                     |              |
                   Begin           End   

           RETURNS: True, if self covers from Begin to End all characters.
                    False, if not.
        """
        Begin = max(Begin, -sys.maxint)
        End   = min(End, sys.maxint)
        if   len(self.__intervals) != 1:        return False
        elif self.__intervals[0].begin > Begin: return False
        elif self.__intervals[0].end < End:     return False
        else:                                   return True

    def cut_interval(self, CutInterval):
        """Adds an interval and ensures that no overlap with existing
        intervals occurs. Note: the 'touch' test is faster here, because
        only one interval is checked against.!"""
        assert CutInterval.__class__ == Interval
        if CutInterval.is_empty(): return
        
        # (*) determine if the interval has any intersection at all
        if    len(self.__intervals) == 0                          \
           or CutInterval.begin >  self.__intervals[-1].end       \
           or CutInterval.end   <= self.__intervals[0].begin:
            # (the cutting interval cannot cut out anything)
            return

        Y = CutInterval
        remainder_low = None
        remainder_up  = None
        # (*) find the first interval with which the cutting interval intersects.
        i = -1
        for x in self.__intervals:
            i += 1
            # (1) an intersecting interval is not yet reached
            if Y.begin >= x.end:    continue                        
            # (2) an intersecting interval was never reached
            #     (the cutting interval cannot cut out anything)
            elif Y.end < x.begin:   return
            # (3) INTERSECTION (implicit from above conditions)
            #     the following conditions are not mutually exclusive.
            #     from now on it is clear that the loop will be left.
            # (3a) the cut leaves a 'lower interval'
            if x.begin < Y.begin:   remainder_low = Interval(x.begin, Y.begin)
            # (3b) the cut leaves an 'upper interval'
            if x.end > Y.end:       remainder_up  = Interval(Y.end, x.end)
            # (3c) the interval has been swallowed completely 
            #      (both remainders stay empty)
            insertion_index = i
            break

        # (*) find the last interval that is concerned with the cut
        toucher_front = i 
        toucher_back  = i

        if remainder_up is None and i != len(self.__intervals) - 1:
            for x in self.__intervals[i+1:]:
                i += 1
                # (1) last interval was swallowed complety, current interval has no intersection
                if Y.end <= x.begin: break
                # (2) INTERSECTION (implicit)
                toucher_back = i
                # (2a) last intersecting interval (probably) not yet reached
                if Y.end > x.end:   continue
                # (2b) last cutting leaves an upper interval
                if Y.end < x.end:    
                    remainder_up  = Interval(Y.end, x.end)
                    break

        # Delete all intervals that touched the 'cut interval'
        del self.__intervals[toucher_front:toucher_back+1]

        # insert the upper remainder first, so that it comes after the lower remainder
        if remainder_up  is not None: self.__intervals.insert(insertion_index, remainder_up)
        if remainder_low is not None: self.__intervals.insert(insertion_index, remainder_low)

    def difference(self, Other):
        assert Other.__class__ == Interval or Other.__class__ == NumberSet

        clone = self.clone()

        if Other.__class__ == Interval: 
            clone.cut_interval(Other)
            return clone

        if len(self.__intervals) == 0:
            return clone

        Begin = self.__intervals[0].begin
        End   = self.__intervals[-1].end
        # note: there should be no internal overlaps according to 'add_interval'
        for interval in Other.__intervals:
            if interval.end   <= Begin: continue
            if interval.begin >= End:   break
            clone.cut_interval(interval)

        return clone

    def symmetric_difference(self, Other):
        """Finds the set of numbers that is either in self or in Other but not
           in both. This corresponds to the operation 
           
                       (self union Other) - (self intersection Other)

           EXAMPLE:

              A     [--------------]    [-------]      [------------]
              B               [--------------]
          
              A|B   [---------------------------]      [------------]
              A&B             [----]    [----]
              A^B   [--------]     [----]    [--]      [------------]
        """
        clone0 = self.clone()
        clone0.unite_with(Other)

        clone1 = self.clone()
        clone1.intersect_with(Other)
        
        clone0.subtract(clone1)
        return clone0

    def complement(self, UniversalSet):
        """Transforms self into NumberSet containing all values which are in 
        UniversalSet but not in self.
        """
        if len(self.__intervals) == 0:
            self.__intervals = UniversalSet.get_intervals()
            return

        first = self.__intervals[0]
        if first.begin != -sys.maxint:
            prev = Interval(-sys.maxint, first.begin)
            self.__intervals.insert(0, prev)
            i    = 1
        else:
            i    = 0

        prev = self.__intervals[i]
        if i < len(self.__intervals) - 1:
            for x in islice(self.__intervals, i+i, None):
                prev.begin = prev.end
                prev.end   = x.begin
                prev       = x

        last = self.__intervals[-1]
        if last.end == sys.maxint:
            del self.__intervals[-1]
        else:
            last.begin = last.end
            last.end   = sys.maxint

        self.intersect_with(UniversalSet)

    def get_complement(self, UniversalSet):
        """RETURNS: NumberSet containing all values X which are in UniversalSet
        but not in self.
        """
        result = self.clone()
        result.complement(UniversalSet)
        return result
        
    @typed(TrafoInfo=list)
    def transform(self, TrafoInfo):
        """Transforms the given NumberSet from into a new NumberSet according 
           to the given TransformationInfo. The TransformationInfo is a list of
           elements consisting of 

           [ SourceInterval_Begin, SourceInterval_End, TargetInterval_Begin ]

           For example an element '[ 32, 49, 256 ]' means that all characters 
           from 32 to 48 are transformed into 256 to 372. The function assumes
           that the entries are sorted with respect to SourceInterval_Begin.

           RETURNS: True  transformation successful
                    False transformation failed, number set possibly in inconsistent state!
        """
        total_verdict = True
        result        = []
        for interval in self.__intervals:
            verdict, transformed = interval.transform(TrafoInfo)
            if verdict == False: total_verdict = False
            result.extend(transformed)
        result.sort(key=lambda x: x.begin)
        self.__intervals = result

        self.clean()
        return total_verdict

    def clean(self, SortF=True):
        """Combines adjacent and intersecting intervals to one.
        """

        # (2) Combine adjacent intervals
        L = len(self.__intervals)
        if L < 2: return

        new_intervals = []
        i = 0 
        current = self.__intervals[0]
        while i < L - 1:
            i += 1
            next = self.__intervals[i]

            if current.end < next.begin: 
                # (1) no intersection?
                new_intervals.append(current)
                current = next
            else:
                # (2) intersection:  [xxxxxxxxxxxxxxx]
                #                              [yyyyyyyyyyyyyyyyy]
                #          becomes   [xxxxxxxxxxxxxxxxxxxxxxxxxxx]
                #
                # => do not append interval i + 1, do not consider i + 1
                if current.end > next.end:
                    # no adaptions necessary, simply ignore next interval
                    pass
                else:
                    # adapt upper border of current interval to the end of the next
                    current.end = next.end

        new_intervals.append(current)

        self.__intervals = new_intervals

    def __repr__(self):
        return repr(self.__intervals)

    def __cmp__(self, Other):
        assert False, "No comparisons defined for class NumberSet"

    def get_intervals(self, PromiseToTreatWellF=False):
        if PromiseToTreatWellF: return self.__intervals
        else:                   return self.__clone_intervals()

    def get_number_list(self):
        """RETURNS: -- List of all numbers which are contained in the number set. 
                    -- None, if one border is 'sys.maxint'. The list would be too big.
        """
        return flatten_list_of_lists(
            xrange(interval.begin, interval.end)
            for interval in self.__intervals
        )

    def get_the_only_element(self):
        if   len(self.__intervals) != 1: return None
        x = self.__intervals[0]
        if x.end - x.begin != 1: return None
        else:                    return x.begin

    def get_string(self, Option="", Delimiter=", "):
        txt = ""
        if len(self.__intervals) == 0:
            return "<empty NumberSet>"
        for interval in self.__intervals:
            txt += interval.get_string(Option, Delimiter) + " "
        return txt

    def get_utf8_string(self):
        msg = ""
        for interval in self.__intervals:
            msg += interval.get_utf8_string() + ", "
        if msg != "": msg = msg[:-2]
        return msg

    def gnuplot_string(self, y_coordinate):
        txt = ""
        for interval in self.__intervals:
            txt += interval.gnuplot_string(y_coordinate)
            txt += "\n"
        return txt

    def assert_consistency(self):
        """Checks whether all intervals are lined up propperly. That is, they
        do not touch and they are sorted from low to high.
        """
        if len(self.__intervals) < 2:
            return
        prev = self.__intervals[0]
        for x in self.__intervals[1:]:
            assert x.begin > prev.end, "%s" % self.__intervals
            prev = x

    def assert_range(self, Minimum, Supremum):
        assert self.minimum()  >= Minimum, \
               "FAIL: %s >= %s" % (self.minimum(), Minimum)
        assert self.supremum() <= Supremum, \
               "FAIL: %s <= %s" % (self.supremum(), Supremum)


# Range of code points that are covered by Unicode
def UnicodeInterval():
    return Interval(0x0, 0x110000)

def NumberSet_All():
    return NumberSet(Interval(-sys.maxint, sys.maxint))


