-module(spider).
-compile(export_all).
-import(lists,[reverse/1,reverse/2,map/2]).

nano_get_url(Host) ->
    {ok,Socket} = gen_tcp:connect(Host, 80, [binary,{packet,0}]),
    ok = gen_tcp:send(Socket,"GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket,[]).

receive_data(Socket,SoFar) ->
    receive
        {tcp,Socket,Bin} ->
            receive_data(Socket,[Bin|SoFar]);
        {tcp_closed,Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.

urls2htmlFile(Urls,File) ->
    file:write_file(File,urls2html(Urls)).

bin2urls(Bin) -> gather_urls(binary_to_list(Bin),[]).

bin2urls2(Bin) -> gather_urls2(binary_to_list(Bin),[]).

urls2html(Urls) -> [h1("Urls"),make_list(Urls)].


h1(Title) -> ["<h1>",Title,"</h1>\n"].

make_list(L) ->
    ["<u1>\n",
        map(fun(I) -> ["<li>",I,"</li>\n"] end, L),
        "</u1>\n"].


gather_urls("<a href" ++ T,L) ->
    {Url,T1}=collect_url_body(T,reverse("<a href")),

%%      case spider1:gather_urls(Url, []) of
%%                              []->
%%                                  nothing;
%%                              [SubUrl]->
%%                                  io:format("SubUrl:~p~n",[SubUrl]),
%%                                  make2(SubUrl)
%%                          end,
    gather_urls(T1,[Url|L]);

gather_urls([_|T],L) ->
    gather_urls(T,L);

gather_urls([],L) ->
    L.

gather_urls2("<a href" ++ T,L) ->
    {Url,T1}=collect_url_body(T,reverse("<a href")),
    gather_urls2(T1,[Url|L]);

gather_urls2("<link href" ++ T,L) ->
    {Url,T1}=collect_url_body(T,reverse("<link href")),
    gather_urls2(T1,[Url|L]);

gather_urls2([_|T],L) ->
    gather_urls2(T,L);
gather_urls2([],L) ->
    L.

collect_url_body("</a>" ++ T,L) -> {reverse(L,"</a>"),T};
collect_url_body(">" ++ T,L) -> {reverse(L,">"),T};
collect_url_body([H|T],L)       -> collect_url_body(T,[H|L]);
collect_url_body([],_)          -> {[],[]}.

make()->
    B=nano_get_url("www.baidu.com"),
    L=bin2urls(B),

    MakeSubFun  =  fun(Url)->
        io:format("Url1:~p~n",[Url]),
        case gather_urls_1(Url, []) of
            []->
                nothing;
            [SubUrl]->
                io:format("SubUrl:~p~n",[SubUrl]),
                make2(SubUrl)
        end
                   end,
    lists:foreach(MakeSubFun , L),
    urls2htmlFile(L,"http\\1.html").

make2(SubUrl)->
    B=nano_get_url(SubUrl),
%%     io:format(B),
    L=bin2urls2(B),
    urls2htmlFile(L,"http\\"++SubUrl++".html").


gather_urls_1("http://" ++ T,L) ->
    {Url,T1}=collect_url_body_1(T,reverse("")),
    gather_urls(T1,[Url|L]);

gather_urls_1([_|T],L) ->
    gather_urls(T,L);
gather_urls_1([],L) ->
    L.

collect_url_body_1("/" ++ T, W) -> {reverse(W,""),T};
collect_url_body_1("\"" ++ T, W) -> {reverse(W,""),T};
collect_url_body_1([Q|T1],W) -> collect_url_body(T1,[Q|W]);
collect_url_body_1([],_) -> {[],[]}.