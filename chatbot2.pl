% has(Symptom) means patient has the specific symptom
:- dynamic has/1.
:- dynamic asked/1.

% a patient definitely does not have a symptom if it has been asked already but the has(Symptom) fact has not been asserted
definitely_does_not_have(Symptom) :- asked(Symptom), \+ has(Symptom).

ask_symptom(Symptom) :-
    format('Does the patient have ~w? ', Symptom),
    read(X),
    assert(asked(Symptom)),
    X == 'y',
    assert(has(Symptom)).

% ensure that the patient didn't say that it doesn't have the symptom
% which will violate the requirement that all symptoms in required_symptoms are present
asked_implies_has(Symptom) :-
    once(has(Symptom); \+ asked(Symptom)).

% ask for symptoms that are not yet known
ask_if_not_asked(Symptom) :-
    once(asked(Symptom); ask_symptom(Symptom)).

% asks until it gets a symptom the patient has
% fails if patient has none
ask_for_one_symptom_patient_has([]) :- false.
ask_for_one_symptom_patient_has([H|T]) :- once(
    (\+ asked(H), ask_symptom(H));
    ask_for_one_symptom_patient_has(T)).


ask_symptom_lists(RequiredSymptomsList, AtLeastOneOfSymptomsList) :-
    % fail if patient is already known to not have ALL of the required symptoms
    maplist(asked_implies_has, RequiredSymptomsList),

    % fail if patient is already known to not have all symptoms in this list
    \+ maplist(definitely_does_not_have, AtLeastOneOfSymptomsList), 

    % asks for the rest of the symptoms, fails if one has a no answer
    maplist(ask_if_not_asked, RequiredSymptomsList),

    % check if a patient is known to have least one of the symptoms in the list
    % if not, ask for one
    once(include(has, AtLeastOneOfSymptomsList, [_|_]);
         ask_for_one_symptom_patient_has(AtLeastOneOfSymptomsList)).
    

diagnosis(diarrhea) :-
    ask_symptom_lists(['loose, watery stools', 'loss of control of bowel movements', 'abdominal pain'],
                      ['nausea', 'high fever', 'chills', 'bloody stools', 'dizziness', 'vomiting']).
diagnosis(lung_tuberculosis) :-
    ask_symptom_lists(['coughing up of blood', 'terrible cough that lasts 3 weeks or longer', 'chest pain'],
                      ['weakness', 'weight loss', 'high fever', 'night sweats', 'no appetite', 'chills']).
diagnosis(dengue) :-
    ask_symptom_lists(['high fever', 'rashes'],
                      ['nausea', 'vomiting', 'headache', 'swollen glands', 'tiredness', 'bleeding from the nose or gums', 'abdominal pain', 'bloody stools']).
diagnosis(pneumonia) :-
    ask_symptom_lists(['cough with phlegm', 'shortness of breath', 'chest pain'],
                      ['high fever', 'chills', 'nausea', 'vomiting', 'no appetite', 'tiredness']).
diagnosis(hepatitis_a) :-
    ask_symptom_lists(['abdominal pain', 'dark-colored urine', 'jaundice'],
                      ['high fever', 'tiredness', 'no appetite', 'nausea']).
diagnosis(acute_bronchitis) :-
    ask_symptom_lists(['coughing with mucus', 'soreness in the chest', 'whistling sound while breathing'],
                      ['tiredness', 'headache', 'muscle pain', 'sore throat', 'shortness of breath', 'low fever']).
diagnosis(malaria) :-
    ask_symptom_lists(['high fever', 'headache', 'chills', 'tiredness', 'muscle pain'],
                      ['nausea', 'vomiting', 'tiredness', 'jaundice', 'abdominal pain']).
diagnosis(typhoid_fever) :-
    ask_symptom_lists(['sustained fever', 'rose-colored spots'],
                      ['weakness', 'abdominal pain', 'headache', 'cough', 'no appetite']).
diagnosis(measles) :-
    ask_symptom_lists(['rashes', 'high fever', 'tiny white spots inside the mouth'],
                      ['cough', 'runny nose', 'red, watery eyes']).
diagnosis(leptospirosis) :-
    ask_symptom_lists(['conjunctival suffusion or the redness of conjunctiva', 'jaundice'],
                      ['high fever', 'headache', 'chills', 'muscle pain', 'vomiting', 'abdominal pain', 'rashes', 'cough']).

print_instructions :-
    format('INSTRUCTIONS:~n1. Type "y." if the answer to the question is yes.~n2. Otherwise, type "n."~n~n~n').

questions_done :-
    \+ diagnosis(_X),
    format('~nThe doctor will need specific test procedures before a diagnosis can be made.~n'),
    format('Please refer to a larger medical facility.~n~n').

main :-
    print_instructions,
    forall(diagnosis(X), format('~nThe patient might have ~w.~n~n', X)),
    (questions_done; true).

:- initialization(main).
