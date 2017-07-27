from quex.engine.misc.enum  import Enum
from quex.DEFINITIONS       import QUEX_PATH

from   itertools   import izip, islice, chain
from   collections import deque
import sys
import os

def r_enumerate(x):
    """Reverse enumeration."""
    return izip(reversed(xrange(len(x))), reversed(x))

def delete_if(the_list, condition):
    for i in xrange(len(the_list)-1, -1, -1):
        if condition(the_list[i]): del the_list[i]

def print_callstack(BaseNameF=False):
    try:
        i = 1
        name_list = []
        while 1 + 1 == 2:
            f = sys._getframe(i)
            x = f.f_code

            # Do not consider the frame coming from the @typed decorator,
            # (See 'def typed(**parameters)' below)
            if x.co_name != "modified": 
                name_list.append([x.co_filename, f.f_lineno, x.co_name])
            i += 1
    except:
        pass

    prev_file_name = ""
    i = - 1
    for x in reversed(name_list):
        if BaseNameF: 
            name = os.path.basename(x[0])
            i += 1
        else:         
            file_name = x[0][len(QUEX_PATH)+1:]
            if file_name != prev_file_name:
                name = file_name
                i += 1
            else:
                # base_name = os.path.basename(x[0])
                base_name = " " * len(os.path.basename(x[0]))
                name = " " * (len(file_name) - len(base_name)) + base_name
            prev_file_name = file_name
            
        print "%s%s:%s:%s(...)" % (" " * (i*4), name, x[1], x[2]) 

def pair_combinations(iterable):
    other = tuple(iterable)
    for i, x in enumerate(other):
        for y in islice(other, i+1, None):
            yield x, y

E_Values = Enum("UNASSIGNED", "VOID", "DISABLED", "SIGMA", "RESTORE", "_DEBUG_E_Values")
# 'SIGMA' is something that is always different. It will cause always the 
# content to become 'VOID', i.e. not uniform in UniformObject.

def concatinate(one_list, other_list):
    """RETURNS: The 'one_list' extended with 'other_list' in case that 
                'other_list' is not None.

       Both lists remain unchanged during the operation.
    """
    if other_list is not None: return one_list + other_list
    else:                      return one_list

def flatten_list_of_lists(ListOfListsIterable):
    """The very fastest way to flatten a list of lists of objects into a list
    of objects.

    EXAMPLE: input:   [1, 2], [3, 4], [5, 6]
             output:  [1, 2, 3, 4, 5, 6]

    RETURNS: List of objects.
    """
    return list(chain.from_iterable(ListOfListsIterable))

def flatten_it_list_of_lists(ListOfListsIterable):
    """Same as 'flatten_list_of_lists' only that an iterable is returned.
    """
    return chain.from_iterable(ListOfListsIterable)

class UniformObject(object):
    __slots__ = ("_content", "_equal")
    def __init__(self, EqualCmp=lambda x,y: x==y, Initial=E_Values.UNASSIGNED):
        if isinstance(Initial, UniformObject):
            self._content = Initial._content
        else:
            self._content = Initial
        self._equal       = EqualCmp

    @staticmethod
    def from_iterable(Iterable, EqualCmp=lambda x,y: x==y):
        try:    initial = Iterable.next()
        except: return UniformObject(EqualCmp, Initial=E_Values.UNASSIGNED)

        result = UniformObject(EqualCmp, Initial=initial)
        for x in Iterable:
            result <<= x
            if result._content == E_Values.VOID: break
        return result

    @staticmethod
    def from_2(A, B, EqualCmp=lambda x,y: x==y):
        result = UniformObject(EqualCmp, Initial=A)
        result <<= B
        return result

    def clone(self):
        result = UniformObject(self._equal)
        result._content = self._content
        return result

    def __ilshift__(self, NewContent):
        if isinstance(NewContent, UniformObject):    
            NewContent = NewContent._content

        if   E_Values.UNASSIGNED == self._content:       self._content = NewContent
        elif E_Values.VOID       == self._content:       pass
        elif E_Values.VOID       == NewContent:          self._content = E_Values.VOID
        elif E_Values.SIGMA      == NewContent:          self._content = E_Values.VOID
        elif not self._equal(self._content, NewContent): self._content = E_Values.VOID
        return self

    def fit(self, NewContent):
        if isinstance(NewContent, UniformObject):    
            NewContent = NewContent._content

        if   E_Values.UNASSIGNED == self._content: return True
        elif E_Values.VOID       == self._content: return False
        
        return self._equal(self._content, NewContent)

    def plain_content(self):
        return self._content

    @property
    def content(self):
        if   E_Values.UNASSIGNED == self._content: return None
        elif E_Values.VOID       == self._content: return None
        else:                                      return self._content

    def is_uniform(self):
        """If the content is UNASSIGNED or remained uniform, then this
           function returns 'True'. It returns 'False' if two different
           values have been shifted into it.
        """
        return E_Values.VOID != self._content

def _report_failed_assertion(i, thing, last_things, iterable_next_things):
    L = len(last_things)
    for k, thing in enumerate(last_things):
        print "[%i](before) \"%s\"" % (i - L + k, thing)

    print ">> [%i] Error: '%s'" % (i, thing.__class__.__name__)
    print ">> [%i] Error: '%s'" % (i, thing)

    for k in xrange(10):
        try:   thing = iterable_next_things.next()
        except StopIteration: break
        print "[%i](after) \"%s\"" % (i + k + 1, thing)

def _check_all(Iterable, Condition):
    if isinstance(Iterable, (int, long, str, unicode)):
        print "#Iterable is not really an iterable"
        return False

    last_things = deque()
    if isinstance(Iterable, (tuple, list)): iterable = Iterable.__iter__()
    else:                                   iterable = Iterable
    i = -1
    while 1 + 1 == 2:
        i     += 1
        try:   thing = iterable.next()
        except StopIteration: break

        if len(last_things) > 10: last_things.popleft()
        last_things.append(thing)
        if Condition(thing): continue
        _report_failed_assertion(i, thing, last_things, iterable)
        return False
    return True

def _get_value_check_function(Type):
    """Tries possible operations on 'Type' and returns the operation which
    works without exception.
    """
    try:     
        if isinstance(4711, Type): pass
        return lambda value: isinstance(value, Type)
    except: 
        if not isinstance(Type, tuple):
            try: 
                if 4711 in Type: pass
                return lambda value: value in Type
            except:
                pass
        else:
            condition_array = tuple( 
                _get_value_check_function(alternative_type) 
                for alternative_type in Type
            )
            def is_ok(element):
                for condition in condition_array:
                    if condition(element): return True
                return False
            return is_ok
    return None

def one_true(Iterable, Condition):
    for x in Iterable:
        if Condition(x) == True: return True
    return False

def all_true(Iterable, Condition):
    for x in Iterable:
        if Condition(x) != True: return False
    return True

def none_true(List, Condition):
    for x in List:
        if Condition(x) == True: return False
    return True

def all_isinstance(List, Type):
    if Type is None: return True
    is_ok = _get_value_check_function(Type) # 'Type' is coded in 'is_ok'
    assert is_ok is not None
    return _check_all(List, is_ok)

def none_isinstance(List, Type):
    if Type is None: return True
    is_ok = _get_value_check_function(Type) # 'Type' is coded in 'is_ok'
    assert is_ok is not None
    return _check_all(List, lambda element: not is_ok(element))

def none_is_None(Iterable):
    return not any(x is None for x in Iterable)

def typed(**_parameters_):
    """parameter=Type                   --> isinstance(parameter, Type)
                                            Type == None --> no requirements.
       parameter=(Type0, Type1, ...)    --> isinstance(parameter, (Type0, Type1, ...))
                                            TypeX == None means that parameter can be None
       parameter=[Type]                 --> (1) isinstance(parameter, list)
                                            (2) all_isinstance(parameter, Type)
       parameter=[(Type0, Type1, ...)]  --> (1) isinstance(parameter, list)
                                            (2) all_isinstance(parameter, (Type0, Type1, ...))
       parameter={Type0: Type1}         --> (1) isinstance(parameter, dict)
                                            (2) all_isinstance(parameter.keys(), Type0)
                                            (3) all_isinstance(parameter.keys(), Type1)
                                        (Here, Type0 or Type1 may be a tuple (TypeA, TypeB, ...)
                                         indicating alternative types.)
    """
    def name_type(TypeD):
        if isinstance(TypeD, tuple):
            return "[%s]" % "".join("%s, " % name_type(x) for x in TypeD)
        elif hasattr(TypeD, __name__):
            return "'%s'" % TypeD.__name__
        else:
            return str(TypeD)

    def error(Name, Value, TypeD):
        return "Parameter '%s' is a '%s'. Expected '%s'." \
               % (Name, Value.__class__.__name__, name_type(TypeD))

    def check_types(_func_, _parameters_ = _parameters_):
        def modified(*arg_values, **kw):
            arg_names = _func_.func_code.co_varnames
            kw.update(zip(arg_names, arg_values))
            for name, type_d in _parameters_.iteritems():
                if name not in kw:  # Default arguments may possibly not appear
                    continue
                value = kw[name]
                if type_d is None:  # No requirements on type_d
                    continue

                elif value is None:
                    assert type_d is None or (type(type_d) == tuple and None in type_d), \
                           error(name, value, type_d)

                elif type(type_d) == tuple:
                    if None in type_d: 
                        # 'None' is accepted as alternative. But, if value was 'None' it
                        # would have triggered the previous case. So, here filter it out.
                        type_d = tuple(set(x for x in type_d if x is not None))
                    assert isinstance(value, type_d), error(name, value, type_d)

                elif type(type_d) == list:
                    assert len(type_d) == 1
                    assert isinstance(value, list), error(name, value, type_d)
                    value_type = type_d[0]
                    assert all_isinstance(value, value_type), error(name, value, type_d)

                elif type(type_d) == dict:
                    assert len(type_d) == 1
                    assert isinstance(value, dict), error(name, value, type_d)
                    key_type, value_type = type_d.iteritems().next()
                    assert all_isinstance(value.iterkeys(), key_type), \
                           "Dictionary '%s' contains key not of of '%s'" % (name, name_type(key_type))
                    assert all_isinstance(value.itervalues(), value_type), \
                           "Dictionary '%s' contains value not of of '%s'" % (name, name_type(value_type))
                else:
                    assert isinstance(value, type_d), \
                           error(name, value, type_d)
            return _func_(**kw)
        return modified
    return check_types

def error_abstract_member():
    x = sys._getframe(1).f_code
    assert False, "Call to '%s'. Implementation in derived class." % x.co_name

class TypedSet(set):
    def __init__(self, Cls):
        self.__element_class = Cls

    def add(self, X):
        assert isinstance(X, self.__element_class)
        set.add(self, X)

    def update(self, Iterable):
        for x in Iterable:
            assert isinstance(x, self.__element_class)
        set.update(self, Iterable)

class TypedDict(dict):
    def __init__(self, ClsKey=None, ClsValue=None):
        self.__key_class   = ClsKey
        self.__value_class = ClsValue

    def get(self, Key):
        assert self.__key_class is None or isinstance(Key, self.__key_class), \
               self._error_key(Key)
        return dict.get(self, Key)

    def __getitem__(self, Key):
        assert self.__key_class is None or isinstance(Key, self.__key_class), \
               self._error_key(Key)
        return dict.__getitem__(self, Key)

    def __setitem__(self, Key, Value):
        assert self.__key_class   is None or isinstance(Key, self.__key_class), \
               self._error_key(Key)
        assert self.__value_class is None or isinstance(Value, self.__value_class), \
               self._error_value(Value)
        return dict.__setitem__(self, Key, Value)

    def update(self, Iterable):
        # Need to iterate twice: 'list()' may be faster here then 'tee()'.
        if isinstance(Iterable, dict): iterable2 = Iterable.iteritems()
        else:                          Iterable = list(Iterable); iterable2 = Iterable.__iter__()

        for x in iterable2:
            assert isinstance(x, tuple)
            assert self.__key_class   is None or isinstance(x[0], self.__key_class), \
                   self._error_key(x[0])
            assert self.__value_class is None or isinstance(x[1], self.__value_class), \
                   self._error_value(x[1])

        dict.update(self, Iterable)

    def _error(self, ExpectedClass):
        return "TypedDict(%s, %s) expects %s" % \
                (self.__key_class.__name__, self.__value_class.__name__, \
                 ExpectedClass.__name__)

    def _error_key(self, Key):
        return "%s as a key. Found type='%s; value='%s';" % \
                (self._error(self.__key_class), Key.__class__.__name__, Key)

    def _error_value(self, Value):
        return "%s as value. Found '%s'" % \
                (self._error(self.__value_class), Value.__class__.__name__)

def return_None(*Any):
    """A function that returns None independent of the number of arguments."""
    return None

def return_empty_list(*Any):
    """A function that returns None independent of the number of arguments."""
    return []
