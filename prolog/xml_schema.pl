:- module(xml_schema,
	  [ xsd_read/2,			% :File, +Options
	    xsd_load/2,			% :DOM, +Options
	    xsd_clean/1,		% :Options
	    xsd_create_element/3,	% +ElementName, +Values, -DOM
	    xsd_type_description/2,	% :DOM, -PrologType
	    xsd_element_documentation/3 % ?Element, ?Type, ?Documentation
	  ]).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(assoc)).
:- use_module(library(apply)).

:- meta_predicate
	xsd_read(:, +),
	xsd_create_element(+, :, -),
	xsd_element_documentation(:, ?, ?),
	xsd_type_description(:, -),
	qualify_dom(+, -, 2, +).

/** <module> Query XML Schema files

Provide a simple mapping  between  Prolog   structures  and  an  XML DOM
structure that satisfies a given XSD type.  Input is

    * The desired element name
    * List of (sub)element name=value.  E.g.
      xml_dom(authenticate, [authId=jan, password=geheim], DOM).
    * Element values can be a list for multiple
    * Element names can be name1/name2/... to disambiguate paths.

How it works:

    - Find the element, create element(Element, Atts, Elements)
    - Find the type declaration
*/

%%	xsd_create_element(+ElementName, :Values, -DOM) is det.
%
%	Create a valid XML DOM, given the name of the outer elements and
%	a list of Name=Value pairs.

xsd_create_element(ElementName, Module:Values, DOM) :-
	var(DOM), !,
	must_be(list, Values),
	maplist(normalize_input, Values, Values1),
	xsd_create_element(ElementName, Values1, RestValues,
			   [], DOM, [module(Module)]),
	assertion(RestValues == []).
xsd_create_element(ElementName, Module:Values, DOM) :-
	xsd_create_element(ElementName, Values, [],
			   [], DOM, [module(Module)]).

normalize_input(Name=Value, Name=Value) :- !.
normalize_input(Term, Name=Value) :-
	Term =.. [Name,Value], !.
normalize_input(Term, _) :-
	type_error(input_value, Term).

xsd_create_element(ElementName,
		   Values, RestValues,
		   Path,
		   Element,
		   Options) :-
	find_element(ElementName, QName, Type, Options),
	create_element(QName, Type, Values, RestValues, Path, Element, Options).

create_element(QName, Type, Values, RestValues, Path, Element, Options) :-
	Element = element(QName, Attrs, Content),
	fill_attributes(Type, Values, RestValues1, Attrs, Options),
	fill_content(Type, RestValues1, RestValues, [QName|Path],
		     Content, Options).


%%	fill_attributes(+Type, +Values, -RestValues, -Attributes, +Options).
%%	fill_attributes(+Type, +Values, -RestValues, +Attributes, +Options).
%
%	Use values to fill attributes from type.
%
%	@tbd	Currently only the fallback clause

fill_attributes(_, Values0, Values, Attributes, _) :-
	var(Attributes), !,
	Attributes = [],
	Values = Values0.
fill_attributes(_, Values0, Values, Attributes, _) :-
	exclude(xmlns_attribute, Attributes, RealAttributes),
	append(RealAttributes, Values, Values0).

xmlns_attribute(xmlns=_).
xmlns_attribute(xmlns:_=_).


%%	fill_content(+Type, +Values, -RestValues, +Path, -Content, +Options)
%
%	Fill the contents

fill_content('http://www.w3.org/2001/XMLSchema':string,
	     Values, RestValues, [_:Name|_], [Value], _Options) :- !,
	(   select(Name=Value, Values, RestValues)
	->  true
	;   existence_error(parameter, Name:'xsd:string')
	).
fill_content(Type, Values, RestValues, Path, Content, Options) :-
	option(module(M), Options),
	call(M:xsd_type(Type, Descr)), !,
	xml_fill_content(Descr, Values, RestValues, Path, Content, Options).

xml_fill_content(Descr, Values, RestValues, Path, Content, Options) :-
	sequence_type(Descr, Seq, Options), !,
	findall(Part, xml_element_info(Seq, Part, Options), Parts),
	content(Parts, Values, RestValues, Path, Content, Options).

%%	xsd_type_description(:XMLDescription, -PrologDescription)
%
%	Transform an XSD type description into a readable Prolog one.
%
%	@tbd	Very incomplete.

xsd_type_description('http://www.w3.org/2001/XMLSchema':boolean, boolean) :- !.
xsd_type_description('http://www.w3.org/2001/XMLSchema':string, atom) :- !.
xsd_type_description('http://www.w3.org/2001/XMLSchema':integer, integer) :- !.
xsd_type_description(Module:XML, Prolog) :-
	xsd_type_description(XML, Prolog, [module(Module)]).

xsd_type_description(XML, Prolog, Options) :-
	sequence_type(XML, Sequence, Options), !,
	Prolog = sequence(Sequence).
xsd_type_description(XML, Prolog, _Options) :-
	enum_type(XML, Values), !,
	Prolog = oneof(Values).


%%	sequence_type(+Description, -Sequence, +Options) is semidet.
%
%	True if Description describes a sequence of elements.  Currently
%	deals with plain sequences and extensions of plain sequences.
%
%	@param Sequence is a DOM element.

sequence_type(Descr, Seq, _) :-
	xpath_chk(Descr, /(_:complexType(self)), _),
	xpath_chk(Descr, _:sequence(self), Seq), !.
sequence_type(Descr, Seq, Options) :-
	xpath_chk(Descr, /(_:complexType(self)), _),
	xpath_chk(Descr, (_:complexContent)/(_:extension(@base=Base)), Ext),
	xpath_chk(Ext, _:sequence(self), ExtSeq),
	option(module(M), Options),
	call(M:xsd_type(Base, BaseDescr)),
	sequence_type(BaseDescr, BaseSeq, Options), !,
	append_content(BaseSeq, ExtSeq, Seq).

append_content(element(Name, Attrs, Content0),
	       element(_, _, ExtraContent),
	       element(Name, Attrs, Content)) :-
	append(Content0, ExtraContent, Content).

%%	enum_type(+Description, -Values) is semidet.
%
%	True  when  Descriptions  describes  an  XSD   type  that  is  a
%	restriction of =string= as an enumeration   and Values is a list
%	holding all enumerations.

enum_type(Descr, Values) :-
	xpath_chk(Descr, /(_:simpleType), _),
	xpath(Descr, /(_:simpleType)/(_:restriction(@base=_:string)), Res), !,
	findall(V, xpath(Res, _:enumeration(@value=V), _), Values).

%%	xml_element_info(+SequenceType, -Info, +Options) is nondet.
%
%	Enumerate info about a sequence of elements.
%
%	@param	Info is a term c(ElementName, Type, ElOpts), where
%		ElOpts is a list of additional options.  Currently
%		defined options are min_occurs(Min) and
%		max_occurs(Max), where Max is =unbounded= or an integer
%		and min is always an integer.

xml_element_info(Seq, Info, Options) :-
	xpath(Seq, _:element, Elem),
	element_info(Elem, Info, Options).

element_info(Elem, c(Name,Type,Opts), _Options) :-
	xpath(Elem, /(_:element(@name=Name,@type=Type)), _),
	Elem = element(_, Attrs, _),
	elem_info(Attrs, Opts).
element_info(Elem, c(Ref,Type,Opts), Options) :-
	xpath(Elem, /(_:element(@ref=Ref)), _),
	option(module(M), Options),
	M:xsd_element(Ref, Type, _), !,
	Elem = element(_, Attrs, _),
	elem_info(Attrs, Opts).

elem_info([], []).
elem_info([H|T], Opts) :-
	elem_info_1(H, Opt), !,
	Opts = [Opt|Rest],
	elem_info(T, Rest).
elem_info([_|T], Opts) :-
	elem_info(T, Opts).

elem_info_1(minOccurs=Atom, min_occurs(N)) :- !,
	(   atom_number(Atom, N)
	->  true
	;   domain_error(number_text, Atom)
	).
elem_info_1(maxOccurs=Atom, max_occurs(N)) :- !,
	(   Atom == unbounded
	->  N = unbounded
	;   atom_number(Atom, N)
	->  true
	;   domain_error(number_text, Atom)
	).


content([], Values, Values, _, [], _).
content([H|T], Values0, Values, Path, Content, Options) :-
	content1(H, Values0, Values1, Path, Content, Tail, Options),
	content(T, Values1, Values, Path, Tail, Options).

content1(c(Name,Type,Opts), Values0, Values1, Path, Content, Tail, Options) :-
	var(Content), !,
	Name = _NS:Local,
	(   complex_type(Type, Options)
	->  create_element(Name, Type, Values0, Values1, [Name|Path],
			   Element, Options),
	    Content = [Element|Tail]
	;   select(Local=Value, Values0, Values1)
	->  (   is_list(Value)
	    ->  check_cardinality(Value, Opts),
		maplist(map_make_element(Type, Name, Options), Value, Elements),
		append(Elements, Tail, Content)
	    ;   make_element(Type, Value, Name, Element, Options),
		Content = [Element|Tail]
	    )
	;   option(min_occurs(0), Opts)
	->  Tail = Content,
	    Values1 = Values0
	;   existence_error(parameter, Name:Type)
	).
content1(c(Name,Type,Opts), Values0, Values1, Path, Content, Tail, Options) :-
	Name = _NS:Local,
	partition(named(Name), Content, ValueElements, Tail),
	(   option(min_occurs(0), Opts),
	    ValueElements == []
	->  Values1 = Values0
	;   complex_type(Type, Options),
	    (   multi_valued(Opts)
	    ->	Values0 = [Local=Values|Values1],
		maplist(element_content(Type, Options, [Name|Path]),
			ValueElements, Values)
	    ;	ValueElements = [element(_,_,SubContent)],
		fill_content(Type, Values0, Values1, [Name|Path],
			     SubContent, Options)
	    )
	->  true
	;   multi_valued(Opts)
	->  Values0 = [Local=Values|Values1],
	    maplist(element_value(Name,Type,Options), ValueElements, Values)
	;   ValueElements = [ValueElement]
	->  Values0 = [Local=Value|Values1],
	    element_value(Name, Type, Options, ValueElement, Value)
	;   assertion(fail)
	).

named(Name, element(Name,_,_)).

multi_valued(Opts) :-
	option(max_occurs(NotOne), Opts), NotOne \== 1.

%%	complex_type(+Type, +Options) is semidet.
%
%	True if Type is	an XSD complex type.

complex_type(Type, Options) :-
	option(module(M), Options),
	M:xsd_type(Type, Descr),
	xpath(Descr, /(_:complexType), _), !.

element_content(Type, Options, Path, element(_,_,Content), Value) :-
	fill_content(Type, Value, [], Path, Content, Options).

element_value(Name, Type, Options, Element0, Value) :-
	strip_xmlns(Element0, Element),
	make_element(Type, Value, Name, Element, Options).

strip_xmlns(element(Name, Atts0, Content),
	    element(Name, Atts,  Content)) :-
	exclude(xmlns_attribute, Atts0, Atts).

map_make_element(Type, Name, Options, Value, Element) :-
	make_element(Type, Value, Name, Element, Options).

%%	make_element(+Type, +Value, +Name, -Element, +Options) is det.
%
%	Create a DOM Element with tag Name, creating the content from
%	Value, given that the content is of type Type.

make_element('http://www.w3.org/2001/XMLSchema':string,
	     Value, Name, element(Name, [], [Value]), _) :- !.
make_element('http://www.w3.org/2001/XMLSchema':language,
	     Value, Name, element(Name, [], [Value]), _) :- !.
make_element('http://www.w3.org/2001/XMLSchema':boolean,
	     Value, Name, element(Name, [], [Value]), _) :- !,
	must_be(boolean, Value).
make_element('http://www.w3.org/2001/XMLSchema':integer,
	     Value, Name, element(Name, [], [Atom]), _) :- !,
	atom_number(Atom, Value).
make_element('http://www.w3.org/2001/XMLSchema':nonNegativeInteger,
	     Value, Name, element(Name, [], [Atom]), _) :- !,
	atom_number(Atom, Value),
	must_be(nonneg, Value).
make_element('http://www.w3.org/2001/XMLSchema':base64Binary,
	     Value, Name, element(Name, [], [Encoded]), _) :- !,
	base64(Value, Encoded).
make_element('http://www.w3.org/2001/XMLSchema':dateTime,
	     Stamp, Name, element(Name, [], [Value]), _) :- !,
	(   nonvar(Value)
	->  parse_time(Value, Stamp)
	;   number(Stamp), Stamp > 3000		% Time stamp, not a year
	->  format_time(atom(Value), '%FT%T%:z', Stamp)
	;   Value = Stamp			% User supplied time
	).
make_element(Type, Value, Name, Element, Options) :-
	option(module(M), Options),
	M:xsd_type(Type, Descr),
	xpath(Descr, /(_:simpleType)/(_:restriction(@base=Base)), _), !,
	make_element(Base, Value, Name, Element, Options).
make_element(Type, Value, Name, element(Name, [], [Value]), _) :-
	(   debugging(soap)
	->  print_message(warning, literal_type(Type))
	;   true
	).

check_cardinality(Value, Options) :-
	option(max_occurs(Max), Options), !,
	(   (   Max == unbounded
	    ;	length(Value, Len),
		Len =< Max
	    )
	->  (   option(min_occurs(Min), Options),
	        Len < Min
	    ->	domain_error(min_occurs(Min), Value)
	    ;	true
	    )
	;   domain_error(max_occurs(Max), Value)
	).
check_cardinality(Value, Options) :-
	option(min_occurs(Min), Options), !,
	length(Value, Len),
	(   Len >= Min
	->  true
	;   domain_error(min_occurs(Min), Value)
	).


%%	xsd_read(:File, +Options) is det.
%
%	Read definitions from File.  Asserts the following facts:
%
%	  * xsd_element(Name, Type, Options)
%	  * xsd_type(Type, Description)
%	    Description is simply the XSD DOM

xsd_read(Module:File, Options) :-
	(   option(namespace(NameSpace), Options)
	->  LoadOptions = [xmlns(NameSpace)]
	;   LoadOptions = []
	),
	load_structure(File, [Schema],
		       [ dialect(xmlns),
			 space(remove)
		       | LoadOptions
		       ]),
	prefix_map(Schema, PrefixMap),
	(   xpath(Schema, /(_:schema(@targetNamespace)), TargetNameSpace)
	->  TSOptions = [target_namespace(TargetNameSpace)]
	;   TSOptions = []
	),
	merge_options([ file(File),
			prefixmap(PrefixMap)
		      | TSOptions
		      ],
		      Options, NewOptions),

	xsd_load(Module:Schema, NewOptions).

prefix_map(element(_, Attrs, _), PrefixMap) :-
	prefix_list(Attrs, Pairs),
	list_to_assoc(Pairs, PrefixMap).

prefix_list([], []).
prefix_list([xmlns:Name=Prefix|T0], [Name-Prefix|T]) :- !,
	prefix_list(T0, T).
prefix_list([xmlns=Prefix|T0], [''-Prefix|T]) :- !,
	prefix_list(T0, T).
prefix_list([_|T0], T) :-
	prefix_list(T0, T).


%%	xsd_load(:Schema, +Options)
%
%	Build XSD types from a parsed XML xsd:schema DOM structure.

xsd_load(Module:Schema, Options) :-
	xsd_clean(Module:Options),
	extract_imports(Schema, Module, Options),
	extract_elements(Schema, Module, Options),
	extract_types(Schema, Module, Options).

xsd_clean(Module:Options) :-
	(   option(cleanup(true), Options, true)
	->  retractall(Module:xsd_element(_,_,_)),
	    retractall(Module:xsd_type(_,_))
	;   true
	).

extract_imports(Schema, Module, Options) :-
	forall(xpath(Schema, _:import(@namespace=NameSpace,
				      @schemaLocation=Import), _),
	       import_schema(Import, NameSpace, Module, Options)).

import_schema(File, NameSpace, Module, Options) :-
	option(file(RelativeTo), Options),
	absolute_file_name(File, Path,
			   [ access(read),
			     relative_to(RelativeTo)
			   ]),
	merge_options([ namespace(NameSpace)
		      ], Options, NewOptions),
	xsd_read(Module:Path, [cleanup(false)|NewOptions]).


extract_elements(Schema, Module, Options) :-
	forall(xpath(Schema, _:element(@name=Name), Element),
	       extract_element(Element, Name, Module, Options)).

extract_element(Element, Name, Module, Options) :-
	xpath(Element, /(_:element(@type)), Type), !,
	qualify_name(Name, QName, Options),
	qualify_name(Type, QType, Options),
	element_options(Element, ElOpts),
	assert_element(QName, QType, ElOpts, Module).
extract_element(Element, Name, Module, Options) :-
	Element = element(_, _, [Description]),
	qualify_name(Name, QName, Options),
	(   QName = Prefix:Local
	->  atomic_list_concat([typeof_, Prefix, :, Local], Type)
	;   assertion(fail)
	),
	qualify_dom(Description, QDescription, qattr, Options),
	element_options(Element, ElOpts),
	assert_element(QName, Type, ElOpts, Module),
	assert_type(Type, QDescription, Module).

assert_element(QName, QType, ElOpts, Module) :-
	Module:xsd_element(QName, QType, ElOpts), !.
assert_element(QName, QType, ElOpts, Module) :-
	assertz(Module:xsd_element(QName, QType, ElOpts)).

assert_type(Type, QDescription, Module) :-
	Module:xsd_type(Type, QDescription), !.
assert_type(Type, QDescription, Module) :-
	assertz(Module:xsd_type(Type, QDescription)).


element_options(Element, Documentation) :-
	findall(documentation(Doc),
		xpath(Element, //(_:documentation(text)), Doc),
		Documentation).

qattr(type, xmlns).
qattr(base, xmlns).
qattr(name, tns).
qattr(ref,  tns).

extract_types(Schema, Module, Options) :-
	forall(xpath(Schema, _:complexType(@name=Type), Description),
	       ( qualify_name(Type, QType, Options),
		 qualify_dom(Description, QDescription, qattr, Options),
		 assertz(Module:xsd_type(QType, QDescription)))),
	forall(xpath(Schema, _:simpleType(@name=Type), Description),
	       ( qualify_name(Type, QType, Options),
		 qualify_dom(Description, QDescription, qattr, Options),
		 assertz(Module:xsd_type(QType, QDescription)))).


%%	find_element(+Name, -QName, -Type, +Options) is det.
%
%	Find an element from Name.
%
%	@error existence_error(xsd_element, Name)

find_element(QName, QName, Type, Options) :-
	option(module(M), Options),
	M:xsd_element(QName, Type, _), !.
find_element(Name, _, _, _) :-
	existence_error(xsd_element, Name).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	qualify_name(+Name, -QName, -Options) is det.

qualify_name(Name, QName, Options) :-
	qualify_name(Name, tns, QName, Options).

qualify_name(Name, _, Prefix:LN, Options) :-
	sub_atom(Name, B, _, A, :), !,
	sub_atom(Name, 0, B, _, NS),
	sub_atom(Name, _, A, 0, LN),
	option(prefixmap(PrefixMap), Options),
	(   get_assoc(NS, PrefixMap, Prefix)
	->  true
	;   existence_error(namespace, NS)
	).
qualify_name(Name, xmlns, Prefix:Name, Options) :- !,
	option(prefixmap(PrefixMap), Options),
	get_assoc('', PrefixMap, Prefix),
	(   Prefix == 'http://www.w3.org/2001/XMLSchema'
	->  true
	;   writeln(Prefix)
	).
qualify_name(Name, tns, Prefix:Name, Options) :-
	option(target_namespace(Prefix), Options).

%%	qualify_dom(+DOM, -QDOM, :Qualify, +Options) is det.
%
%	Qualify attributes in DOM for which call(Qualify, Attr) is try.

qualify_dom(element(Name,  Attrs,  Content),
	    element(Name, QAttrs, QContent),
	    Qualify, Options) :- !,
	maplist(qualify_attr(Qualify, Options), Attrs, QAttrs),
	qualify_content(Content, QContent, Qualify, Options).
qualify_dom(DOM, DOM, _, _).

qualify_attr(Qualify, Options, Name=Value, Name=QValue) :-
	atom(Value),
	call(Qualify, Name, How), !,
	qualify_name(Value, How, QValue, Options).
qualify_attr(_, _, Attr, Attr).

qualify_content([], [], _, _).
qualify_content([H0|T0], [H|T], Qualify, Options) :-
	qualify_dom(H0, H, Qualify, Options),
	qualify_content(T0, T, Qualify, Options).


		 /*******************************
		 *	  DOCUMENTATION		*
		 *******************************/

%%	xsd_element_documentation(:Element, ?Type, ?Doc)
%
%	Element is the local name (i.e., without namespace)

xsd_element_documentation(Module:Element, Type, Doc) :-
	Term = element_documentation(Element, Type, Doc, Module),
	setof(Term, Term, Results),
	member(Term, Results).

element_documentation(Element, Type, Doc, Module) :-
	Module:xsd_element(_:Element, TypeName, Doc),
	type_description(TypeName, Type, Module).
element_documentation(Element, Type, Doc, Module) :-
	Module:xsd_type(_Type, Descr),
	xpath(Descr, //(_:element(@name=(_:Element), @type=TypeName)), DOM),
	element_options(DOM, Doc),
	type_description(TypeName, Type, Module).

type_description(TypeName, Type, Module) :-
	Module:xsd_type(TypeName, TypeDOM), !,
	(   xsd_type_description(Module:TypeDOM, Type)
	->  true
	;   Type = TypeDOM
	).
type_description(TypeName, Type, _) :-
	xsd_type_description(TypeName, Type), !.
type_description(TypeName, TypeName, _).

