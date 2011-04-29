:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).

:- rdf_register_ns(rss, 'http://purl.org/rss/1.0/').
:- rdf_register_ns(dc, 'http://purl.org/dc/elements/1.1/').

rss_to_html_list(Resource, Source, Target) :-
	rdf_load(Source),
	rdf(Resource, rss:title, literal(Title)),
	phrase(rss_html_list(Title), TargetHtml),
	tell(Target),
	print_html(TargetHtml),
	told.

rss_html_list(ChannelTitle) -->
	page(\rss_list_head(ChannelTitle),
	     \rss_list_body(ChannelTitle)).

rss_list_head(PageTitle) -->
	html([title([PageTitle])]).
rss_list_body(Header) -->
	{ findall(Item, rdf(Item, rdf:type, rss:item), Items),
	  sort(Items, Unique)
	},
	html([h2(align(center),[Header]),
	      ul(\rss_list_items(Unique))]).

rss_list_items([]) -->
	[].
rss_list_items([First_item|Rest_of_items]) -->
	html([li([\list_item_content(First_item)])]),
	rss_list_items(Rest_of_items).

list_item_content(Item) -->
	{rdf(Item,dc:description,literal(Description))},
	html([\rss_link(Item),br([]),Description]).

rss_link(Item) -->
	{ rdf(Item,rss:link,literal(Link)),
	  rdf(Item,rss:title,literal(Title))
	},
	html(i(a(href(Link),Title))).
