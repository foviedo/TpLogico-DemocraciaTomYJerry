% TP Lógico 2019 - Democracia 
% Paradigmas de Programación
%   Grupo: Tom y Jerry

% 1 - Candidatos y Partidos

% Candidatos
candidato(frank).
candidato(claire).
candidato(garrett).
candidato(peter).
candidato(jackie).
candidato(linda).
candidato(catherine).
candidato(seth).
candidato(heather).

% Relación Candidato - Partido
esCandidatoDe(frank, rojo).
esCandidatoDe(claire, rojo).
esCandidatoDe(garrett, azul).
esCandidatoDe(jackie, amarillo).
esCandidatoDe(linda, azul).
esCandidatoDe(catherine, rojo).
esCandidatoDe(seth, amarillo).
esCandidatoDe(heather, amarillo).
% Por principio de universo cerrado (PUC), no es necesario aclarar que peter no es de Amarillo.

% Relación Candidato - Edad
edad(frank, 50).
edad(claire, 52).
edad(garrett, 64).
edad(peter, 26).
edad(jackie, 38).
edad(linda, 30).
edad(catherine, 59).
edad(heather, Edad) :-
    Edad is 2019 - 1969.

% El partido violeta se menciona por PUC

% Relación Partido - Provincia en la que se postula.
sePostulaEn(azul, buenosAires).
sePostulaEn(azul, chaco).
sePostulaEn(azul, tierraDelFuego).
sePostulaEn(azul, sanLuis).
sePostulaEn(azul, neuquen).
sePostulaEn(rojo, buenosAires).
sePostulaEn(rojo, santaFe).
sePostulaEn(rojo, cordoba).
sePostulaEn(rojo, chubut).
sePostulaEn(rojo, tierraDelFuego).
sePostulaEn(rojo, sanLuis).
sePostulaEn(amarillo, buenosAires).
sePostulaEn(amarillo, chaco).
sePostulaEn(amarillo, formosa).
sePostulaEn(amarillo, tucuman).
sePostulaEn(amarillo, salta).
sePostulaEn(amarillo, santaCruz).
sePostulaEn(amarillo, laPampa).
sePostulaEn(amarillo, corrientes).
sePostulaEn(amarillo, misiones).
% Que el rojo no se presente en formosa no se menciona por PUC.

% Relación Provincia - Cantidad de Habitantes
habitantes(buenosAires, 15355000).
habitantes(chaco, 1143201).
habitantes(tierraDelFuego, 160420).
habitantes(sanLuis, 489255).
habitantes(neuquen, 637913).
habitantes(santaFe, 3397532).
habitantes(cordoba, 3567654).
habitantes(chubut, 577466).
habitantes(formosa, 527895).
habitantes(tucuman, 1687305).
habitantes(salta, 1333365).
habitantes(santaCruz, 273964).
habitantes(laPampa, 349299).
habitantes(corrientes, 992595).
habitantes(misiones, 189446).

% Relación Provincia - Partido - Porcentaje de Votos
intencionDeVotoEn(buenosAires, rojo, 40).
intencionDeVotoEn(buenosAires, azul, 30).
intencionDeVotoEn(buenosAires, amarillo, 30).
intencionDeVotoEn(chaco, rojo, 50).
intencionDeVotoEn(chaco, azul, 20).
intencionDeVotoEn(chaco, amarillo, 0).
intencionDeVotoEn(tierraDelFuego, rojo, 40).
intencionDeVotoEn(tierraDelFuego, azul, 20).
intencionDeVotoEn(tierraDelFuego, amarillo, 10).
intencionDeVotoEn(sanLuis, rojo, 50).
intencionDeVotoEn(sanLuis, azul, 20).
intencionDeVotoEn(sanLuis, amarillo, 0).
intencionDeVotoEn(neuquen, rojo, 80).
intencionDeVotoEn(neuquen, azul, 10).
intencionDeVotoEn(neuquen, amarillo, 0).
intencionDeVotoEn(santaFe, rojo, 20).
intencionDeVotoEn(santaFe, azul, 40).
intencionDeVotoEn(santaFe, amarillo, 40).
intencionDeVotoEn(cordoba, rojo, 10).
intencionDeVotoEn(cordoba, azul, 60).
intencionDeVotoEn(cordoba, amarillo, 20).
intencionDeVotoEn(chubut, rojo, 15).
intencionDeVotoEn(chubut, azul, 15).
intencionDeVotoEn(chubut, amarillo, 15).
intencionDeVotoEn(formosa, rojo, 0).
intencionDeVotoEn(formosa, azul, 0).
intencionDeVotoEn(formosa, amarillo, 0).
intencionDeVotoEn(tucuman, rojo, 40).
intencionDeVotoEn(tucuman, azul, 40).
intencionDeVotoEn(tucuman, amarillo, 20).
intencionDeVotoEn(salta, rojo, 30).
intencionDeVotoEn(salta, azul, 60).
intencionDeVotoEn(salta, amarillo, 10).
intencionDeVotoEn(santaCruz, rojo, 10).
intencionDeVotoEn(santaCruz, azul, 20).
intencionDeVotoEn(santaCruz, amarillo, 30).
intencionDeVotoEn(laPampa, rojo, 25).
intencionDeVotoEn(laPampa, azul, 25).
intencionDeVotoEn(laPampa, amarillo, 40).
intencionDeVotoEn(corrientes, rojo, 30).
intencionDeVotoEn(corrientes, azul, 30).
intencionDeVotoEn(corrientes, amarillo, 10).
intencionDeVotoEn(misiones, rojo, 90).
intencionDeVotoEn(misiones, azul, 0).
intencionDeVotoEn(misiones, amarillo, 0).

% 2 - Provincia Picante

esPicante(Provincia) :- % Se cumple cuando al menos dos partidos presentan candidatos en dicha provincia y además esta tiene más de 1 millón de habitantes.
    sePostulaEn(Partido1, Provincia),
    sePostulaEn(Partido2, Provincia),
    Partido1 \= Partido2,
    habitantes(Provincia, Habitantes),
    Habitantes > 1000000.

% 3 - PASO
candidatoQueSePresentaEn(Candidato,Partido,Provincia):-
    esCandidatoDe(Candidato,Partido),
    sePostulaEn(Partido,Provincia).

leGanaA(Candidato1,Candidato2,Provincia) :- % Se cumple si candidato le gana a otro en una provincia.
    candidatoQueSePresentaEn(Candidato1,Partido1,Provincia),
    esCandidatoDe(Candidato2, Partido2),
    not(sePostulaEn(Partido2, Provincia)).
        
leGanaA(Candidato1,Candidato2,Provincia) :- % Se cumple si un candidato le gana a otro en una provincia.
    candidatoQueSePresentaEn(Candidato1,Partido1,Provincia),
    candidatoQueSePresentaEn(Candidato2,Partido2,Provincia),
    tieneMasVotos(Partido1, Partido2, Provincia).

leGanaA(Candidato1,Candidato2,Provincia) :- % Se cumple si un candidato le gana a otro en una provincia.
    candidatoQueSePresentaEn(Candidato1,Partido,Provincia),
    candidatoQueSePresentaEn(Candidato2,Partido,Provincia).

tieneMasVotos(Partido1,Partido2,Provincia) :-
    intencionDeVotoEn(Provincia, Partido1, Intencion1),
    intencionDeVotoEn(Provincia, Partido2, Intencion2),
    Intencion1 > Intencion2.

% 4 - El Gran Candidato

elGranCandidato(Candidato) :- % Se cumple para un candidato si le gana a todos los demás candidatos de las provincias que se postulan y además es el más joven.
    esCandidatoDe(Candidato, Partido),
    forall(sePostulaEn(Partido, Provincia), partidoGanaEnProvincia(Partido, Provincia)),
    forall(esCandidatoDe(OtroCandidato, Partido), esMasJoven(Candidato, OtroCandidato)).

partidoGanaEnProvincia(Partido, Provincia):-
    sePostulaEn(Partido, Provincia),
    forall(sePostulaUnPartidoDistinto(Partido, OtroPartido, Provincia), tieneMasVotos(Partido, OtroPartido, Provincia)).

sePostulaUnPartidoDistinto(Partido, OtroPartido, Provincia) :-
    sePostulaEn(OtroPartido, Provincia),
    Partido \= OtroPartido.

esMasJoven(Candidato, OtroCandidato):-
    edad(Candidato, Edad1),
    edad(OtroCandidato, Edad2),
    Edad1 =< Edad2.
% Se puede consultar quién es el gran candidato, poniendo desde el Swi Prolog elGranCandidato(Candidato)., que responderá frank, esto es posible gracias a la inversibilidad del predicado

% 5 - Malas Consultoras

ajusteConsultora(Partido, Provincia, VerdaderoPorcentaje) :-
    partidoGanariaEnProvincia(Partido, Provincia),
    intencionDeVotoEn(Provincia, Partido, Intencion),
    VerdaderoPorcentaje is Intencion - 20.

ajusteConsultora(Partido, Provincia, VerdaderoPorcentaje) :-
    not(partidoGanariaEnProvincia(Partido, Provincia)),
    intencionDeVotoEn(Provincia, Partido, Intencion),
    VerdaderoPorcentaje is Intencion + 5.

partidoGanariaEnProvincia(Partido,Provincia) :-
    sePostulaEn(Partido, Provincia),
    forall(intencionDeVotoDistintosPartidos(Provincia, Partido, OtroPartido), tieneMasVotos(Partido, OtroPartido, Provincia)).

intencionDeVotoDistintosPartidos(Provincia, Partido, OtroPartido) :-
    intencionDeVotoEn(Provincia, OtroPartido, _),
    Partido \= OtroPartido.

% En leGanaA y elGranCandidato se compara quiénes tienen más votos a partir de las intenciones de voto, en este caso, se debería modificar ambas para que sus intenciones pasen por ajusteConsultora así saber bien quién gana.

% 6 - Promesas de Campaña

promete(azul, construir([edilicio(hospital, 1000), edilicio(jardin, 100), edilicio(escuela, 5)])).
promete(amarillo, construir([edilicio(hospital, 100), edilicio(universidad, 1), edilicio(comisaria, 200)])).
promete(rojo, nuevosPuestosDeTrabajo(800000)).
promete(amarillo, nuevosPuestosDeTrabajo(10000)).
promete(azul, inflacion(2, 4)).
promete(amarillo, inflacion(1, 15)).
promete(rojo, inflacion(10, 30)).

% 7 - Ajustes de Boca de Urna

influenciaDePromesas(inflacion(CotaInferior, CotaSuperior), Variacion) :-
    Variacion is - ((CotaSuperior + CotaInferior) / 2).

influenciaDePromesas(nuevosPuestosDeTrabajo(Puestos),3) :-
    Puestos > 50000.

influenciaDePromesas(construir(Lista), Variacion) :-
    construyeHospitales(Lista, VariacionHospitales),
    construyeJardinesOEscuelas(Lista, VariacionJOE),
    construyeComisarias(Lista, VariacionComisarias),
    construyeInnecesarios(Lista, VariacionInnecesarios),
    Variacion is VariacionHospitales + VariacionJOE + VariacionComisarias - VariacionInnecesarios.

construyeHospitales(Lista, 2) :-
    member(edilicio(hospital, _), Lista).

construyeHospitales(Lista, 0) :-
    not(member(edilicio(hospital, _), Lista)).

construyeJardinesOEscuelas(Lista,VariacionJOE) :-
    findall(Cantidad, tieneJardinOEscuela(Lista, Cantidad), Cantidades),
    sumlist(Cantidades, Suma),
    VariacionJOE is 0.1 * Suma.

tieneJardinOEscuela(Lista, Cantidad) :-
    member(edilicio(escuela, Cantidad), Lista).

tieneJardinOEscuela(Lista, Cantidad) :-
    member(edilicio(jardin, Cantidad), Lista).

construyeComisarias(Lista, 2) :-
    member(edilicio(comisaria,200), Lista).

construyeComisarias(Lista, 0) :-
    not(member(edilicio(comisaria, 200), Lista)).

construyeInnecesarios(Lista, VariacionInnecesarios) :-
    findall(Edilicio, esInnecesario(Lista, Edilicio), EdiliciosInnecesarios),
    length(EdiliciosInnecesarios, VariacionInnecesarios).

esInnecesario(Lista, UnEdilicio) :-
    member(edilicio(UnEdilicio, _), Lista),
    UnEdilicio \= hospital,
    UnEdilicio \= escuela,
    UnEdilicio \= jardin,
    UnEdilicio \= comisaria,
    UnEdilicio \= universidad.

% 8 - Nuevos Votos

promedioDeCrecimiento(Partido,Sumatoria) :-
    sePostulaEn(Partido, _),
    findall(Crecimiento, promesaDePartido(Partido, Crecimiento), Crecimientos),
    sumlist(Crecimientos, Sumatoria).

promesaDePartido(Partido, Crecimiento) :-
    promete(Partido, Promesa),
    influenciaDePromesas(Promesa, Crecimiento).