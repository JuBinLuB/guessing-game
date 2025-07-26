/*
Jogo de identificação de personagens em Prolog.
Autor: Alexssander F. Cândido, Paulo L. Mendes
Data:
*/

% -----------------------------
% BASE DE DADOS DE PERSONAGENS
% -----------------------------

% Base de dados de personagens.
% Cada personagem é representado como character(Nome, Tipo, Origem, [Atributos])
% Onde:
% - Nome: nome do personagem.
% - Tipo: 'ficticio' ou 'real'.
% - Origem: local de origem do personagem.
% - [Atributos]: lista de características adicionais do personagem.

% Personagens fictícios.
character('Harry Potter', ficticio, 'Inglaterra', ['bruxo', 'usa óculos', 'protagonista', 'jovem', 'valente', 'britânico']).
character('Peter Parker', ficticio, 'Estados Unidos', ['super herói', 'fotógrafo', 'usa óculos', 'jovem', 'tímido', 'nerd']).
character('Clark Kent', ficticio, 'Estados Unidos', ['super herói', 'jornalista', 'voa', 'super força', 'amável', 'disfarçado']).
character('Bruce Wayne', ficticio, 'Estados Unidos', ['super herói', 'rico', 'detetive', 'usa capa', 'estrategista', 'humano']).
character('Tony Stark', ficticio, 'Estados Unidos', ['engenheiro', 'rico', 'gênio', 'usa armadura', 'sarcastico', 'inventor']).
character('Diana Prince', ficticio, 'Temiscira', ['super herói', 'guerreira', 'princesa', 'super força', 'imortal', 'mulher']).
character('Homer Simpson', ficticio, 'Springfield', ['personagem animado', 'pai de família', 'comediante', 'preguiçoso', 'americano', 'trabalhador']).
character('Elsa', ficticio, 'Arendelle', ['princesa', 'protagonista', 'personagem animado', 'super poder', 'canta', 'introvertida', 'loira']).
character('Goku', ficticio, 'Terra', ['lutador', 'protagonista', 'super força', 'alienígena', 'faminto', 'determinado']).
character('Mario', ficticio, 'Reino dos Cogumelos', ['personagem de jogo', 'encanador', 'usa boné', 'herói', 'bigode', 'italiano']).

% Personagens reais.
character('Albert Einstein', real, 'Alemanha', ['cientista', 'físico', 'gênio', 'prêmio Nobel', 'alemão', 'professor']).
character('Marie Curie', real, 'Polônia', ['cientista', 'química', 'prêmio Nobel', 'mulher', 'pesquisadora', 'famosa']).
character('Isaac Newton', real, 'Inglaterra', ['cientista', 'físico', 'matemático', 'renomado', 'britânico', 'introspectivo']).
character('Leonardo da Vinci', real, 'Itália', ['artista', 'inventor', 'pintor', 'renascentista', 'gênio', 'multitalentoso']).
character('Frida Kahlo', real, 'México', ['artista', 'pintora', 'expressiva', 'mulher', 'sofrimento', 'reconhecida mundialmente']).
character('Malala Yousafzai', real, 'Paquistão', ['ativista', 'educação', 'jovem', 'prêmio Nobel', 'corajosa', 'feminista']).
character('Pelé', real, 'Brasil', ['atleta', 'jogador de futebol', 'brasileiro', 'copa do mundo', 'negro']).
character('Usain Bolt', real, 'Jamaica', ['atleta', 'velocista', 'olímpico', 'recordista mundial', 'carismático', 'negro']).
character('Barack Obama', real, 'Estados Unidos', ['político', 'presidente', 'advogado', 'prêmio Nobel', 'negro', 'orador']).
character('Greta Thunberg', real, 'Suécia', ['ativista', 'clima', 'jovem', 'palestrante', 'autista', 'famosa']).

% -------------------------------
% PREDICADOS AUXILIARES
% -------------------------------

% Obtém todos os atributos de todos os personagens.
get_attributes(Characters, Attributes) :-
    findall(Attr, 
            (member(character(_, _, _, Attrs), Characters), member(Attr, Attrs)), 
            Attributes).

% Verifica se um personagem tem um determinado atributo.
has_attribute(Attr, character(_, _, _, Attrs)) :- 
    member(Attr, Attrs).

/*
% Conta quantas vezes um atributo aparece nos personagens
count_occurrence(_, [], 0).
count_occurrence(A, [character(_, _, _, Attrs)|T], Count) :-
    count_occurrence(A, T, Rest),
    (member(A, Attrs) -> Count is Rest + 1 ; Count is Rest).

% Conta a frequência de todos os atributos (exceto os já perguntados)
count_attributes([], _, []).
count_attributes([A|T], Characters, [A-C|Rest]) :-
    count_occurrence(A, Characters, C),
    count_attributes(T, Characters, Rest).

% Encontra o atributo mais frequente entre os personagens restantes
most_frequent_attribute(Characters, AskedAttrs, Attr, Count) :-
    get_attributes(Characters, AllAttrs),
    subtract(AllAttrs, AskedAttrs, UnaskedAttrs),
    count_attributes(UnaskedAttrs, Characters, Counted),
    sort(2, @>=, Counted, [Attr-Count|_]).  % maior frequência primeiro
*/

% Encontra o atributo mais discriminativo (que divide os personagens de forma mais equilibrada)
best_discriminatory_attribute(Chars, Asked, BestAttr) :-
    get_attributes(Chars, AllAttrs),
    subtract(AllAttrs, Asked, Unasked),
    maplist(attr_discrimination(Chars), Unasked, AttrScores),
    keysort(AttrScores, Sorted),
    last(Sorted, BestAttr-_).  % Pega o atributo com maior poder discriminatório

attr_discrimination(Chars, Attr, Attr-Score) :-
    include(has_attribute(Attr), Chars, With),
    length(Chars, Total),
    length(With, Count),
    Without is Total - Count,
    Score is min(Count, Without).  % Quanto mais balanceado, melhor

% -----------------------------------
% SISTEMA PRINCIPAL DE INICIALIZAÇÃO
% -----------------------------------

% Predicado principal para iniciar o jogo.
startGame :-
    write('Bem-vindo ao Jogo de Identificação de Personagens!'), nl,
    write('Pense em um personagem (real ou fictício) e eu tentarei adivinhar.'), nl,
    write('Responda com "sim." ou "nao." (incluindo o ponto final) para as características do seu personagem.'), nl, nl,
    prompt_continue,
    !.

% Confirma se o usuário deseja continuar.
prompt_continue :-
    write('Deseja continuar? (sim./nao.) '),
    read(Answer),
    handle_answer(Answer).

% Caso deseje jogar.
handle_answer(sim) :-
    nl,
    write('Ótimo! Vamos começar...'), nl, nl,
    game(10).

% Caso não deseje jogar.
handle_answer(nao) :-
    nl,
    write('Tudo bem. Até a próxima!'), nl, !.

% Caso a resposta seja inválida.
handle_answer(_) :-
    nl,
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl, nl,
    prompt_continue.

% --------------------------
% SISTEMA PRINCIPAL DO JOGO
% --------------------------

% Estado do jogo: game(Attempts, FilteredCharacters, AskedAttributes)
% Onde:
% - Attempts: tentativas restantes
% - FilteredCharacters: personagens ainda possíveis
% - AskedAttributes: atributos já perguntados

% Inicia o jogo com estado inicial.
game(Attempts) :-
    ask_type(Type),
    findall(character(Name, Type, Home, Attrs), 
            character(Name, Type, Home, Attrs), 
            FilteredChars),
    game(Attempts, FilteredChars, []).

% Caso base: sem tentativas restantes.
game(0, _, _) :-
    write('Você venceu! Não consegui adivinhar seu personagem.'), nl, !.
%    learn_character.

% Caso com apenas um personagem possível - adivinhamos.
game(_, [character(Name, _, _, _)], _) :-
    write('Seu personagem é '), write(Name), write('? (sim./nao.) '),
    read(Answer),
    (Answer = sim ->
        write('Acertei!'), nl
    ;
        write('Você venceu! Não consegui adivinhar seu personagem.'), nl
%        learn_character
    ), !.

% Caso geral: faz perguntas.
game(Attempts, RemainingChars, AskedAttrs) :-
    Attempts > 0,
    NewAttempts is Attempts - 1,

%    most_frequent_attribute(RemainingChars, AskedAttrs, Attr, _),
    best_discriminatory_attribute(RemainingChars, AskedAttrs, Attr),

    % Faz a pergunta.
    ask_question(Attr, Answer),

    % Processa a resposta.
    process_attribute_answer(
        Answer, 
        Attr, 
        RemainingChars, 
        AskedAttrs, 
        NewFiltered, 
        NewAsked),

    % Continua o jogo.
    game(NewAttempts, NewFiltered, NewAsked).

% ---------------------
% SISTEMA DE PERGUNTAS
% ---------------------

% Pergunta se o personagem é fictício ou real.
ask_type(Type) :-
    write('Seu personagem é fictício? (sim./nao.) '),
    read(Answer),
    process_type_answer(Answer, Type).

% Interpreta a resposta sobre o tipo.
process_type_answer(sim, ficticio).
process_type_answer(nao, real).
process_type_answer(_, Type) :-
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl,
    ask_type(Type).

ask_question(Attr, Answer) :-
    % Casos específicos para atributos problemáticos
    (   Attr = 'usa óculos'       -> Question = 'Seu personagem usa óculos?'
    ;   Attr = 'super força'      -> Question = 'Seu personagem tem super força?'
    ;   Attr = 'copa do mundo'    -> Question = 'Seu personagem tem copa do mundo?'
    ;   Attr = 'prêmio Nobel'     -> Question = 'Seu personagem tem prêmio Nobel?'
    ;   Attr = 'protagonista'     -> Question = 'Seu personagem é o protagonista?'
    ;   Attr = 'fuma cachimbo'    -> Question = 'Seu personagem fuma cachimbo?'

    % Regra padrão para os demais casos
    ;   format(atom(Question), 'Seu personagem é ~w?', [Attr])
    ),
    write(Attr), write('? (sim./nao.) '),
    read(Answer).

% Predicado para processar a resposta do usuário sobre um atributo.
process_attribute_answer(sim, Attr, Chars, Asked, NewChars, NewAsked) :-
%    write('Ok, seu personagem '), write(Attr), write('.'), nl, nl,
    include(has_attribute(Attr), Chars, NewChars),
    NewAsked = [Attr|Asked].

process_attribute_answer(nao, Attr, Chars, Asked, NewChars, NewAsked) :-
%    write('Ok, seu personagem não '), write(Attr), write('.'), nl, nl,
    exclude(has_attribute(Attr), Chars, NewChars),
    NewAsked = [Attr|Asked].

process_attribute_answer(_, _, Chars, Asked, NewChars, NewAsked) :-
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl,
    NewChars = Chars,
    NewAsked = Asked.

% ---------------------------------
% APRENDIZADO DE NOVOS PERSONAGENS
% ---------------------------------
