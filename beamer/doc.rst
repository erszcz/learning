Algorytmy równoległe: Dekompozycja funkcjonalna i branch and bound
==================================================================

Radosław Szymczyszyn <r.szymczyszyn@gmail.com>

.. .. role:: m(raw)
..     :format: latex html


Problem do rozwiązania: przydział maszyn wirtualnych do zadań w chmurze obliczeniowej
-------------------------------------------------------------------------------------
Dane jest *N* zadań o znanym czasie wykonania *t(1) .. t(N)*
oraz ograniczenie czasowe *D* (deadline).
Należy znaleźć taki przydział zadań do wirtualnych maszyn,
aby koszt wykonania obliczenia był jak najmniejszy,
oraz aby wszystkie zadania zakończyły się przed upływem terminu *D*.
Jedna maszyna może w jednej chwili wykonywać tylko jedno zadanie.
Koszt wirtualnej maszyny pobierany jest za każdą rozpoczętą godzinę
jej działania, niezależnie od tego,
czy jakieś zadanie jest na niej wykonywane.
Wszystkie maszyny posiadają jednakową wydajność i koszt.


PCAM - projekt rozwiązania
--------------------------
Problem zadania jest problemem przeszukiwania dużej przestrzeni
rozwiązań.
Przestrzeń ma kształt drzewa,
którego każdy węzeł reprezentuje pewne rozwiązanie cząstkowe, na
przykład:

:math:`maszyny = \{\{3, 5\}, \{2, 1\}\} \quad zadania = \{5, 3\}`

Rozwiązanie cząstkowe to pewien przydział zadań do maszyn,
na których są liczone oraz zestaw nieprzydzielonych zadań.
Zadanie identyfikowane są ich czasem wykonania w minutach.

Każdy liść reprezentuje rozwiązanie pełne, tj. takie, gdzie brak już
nieprzydzielonych zadań:

:m:`$$ maszyny = \{\{3, 5, 5\}, \{2, 1, 3\}\} \quad zadania = \empty $$`

Korzeń drzewa przedstawić możemy jako:

:m:`$$ maszyny = \empty \quad zadania = \{3, 2, 5, 1, 5, 3\} $$`

Rozgałęzienie w każdym węźle dokonywane jest w następujący sposób:

1. Weź jedno nieprzydzielone zadanie.
2. Dla każdej maszyny w obecnym przyporządkowaniu utwórz węzeł potomny,
   ale do danej maszyny w nowym węźle dołącz zad. wybrane w pkt. 1.
3. Utwórz jeszcze jeden węzeł potomny.
   Jego zbiór przyporządkowań to zbiór węzła obecnego rozszerzony
   o nową maszynę z jednym zadaniem wybranym w pkt. 1.

Przykład - dla węzła postaci:

:m:`$$ maszyny = \{\{3, 5\}, \{2, 1\}\} \quad zadania = \{5, 3\} $$`

Zbiór dzieci wyglądał będzie następująco:

:m:`$$ maszyny = \{\{3, 5, 5\}, \{2, 1\}\} \quad zadania = \{3\} $$`
:m:`$$ maszyny = \{\{3, 5\}, \{2, 1, 5\}\} \quad zadania = \{3\} $$`
:m:`$$ maszyny = \{\{3, 5\}, \{2, 1\}, \{5\}\} \quad zadania = \{3\} $$`

W celu rozwiązania zadania zastosowane zostanie podejście przeszukiwania
wgłąb.
Ze względu na rozmiar przestrzeni,
niepraktyczne jest przeglądanie wszystkich możliwych rozwiązań.
Określone są zatem trzy kryteria odcinania poddrzew,
sprawdzane w momencie wejścia do nowego węzła:

- Jeśli czas wykonania obecnego węzła przekracza deadline *D*,
  to przerwij przeszukiwanie tej gałęzi.

- Jeśli najlepsze dotychczas znalezione rozwiązanie jest najlepszym
  osiągalnym rozwiązaniem, tzn. że spełnia równanie:

  :m:`$$ C = \lceil \frac{\sum_{i = 1 .. N} t_i}{60} \rceil $$`

  gdzie:

  * *C* - koszt rozwiązania,
  * :m:`$ t_i $` - czas wykonania zadania *i*,
    gdzie *i* określa zadanie z początkowego zbioru zadań
    do przyporządkowania,
  * stała 60 oznacza długość jednostki czasu (tutaj godzinę),
    za która naliczany jest jeden punkt kosztu zadania.

  Innymi słowy, jeśli znalezione rozwiązanie osiągnęło minimalny koszt
  obliczeń możliwy przy takim zestawie zadań wejściowych,
  to przerwij poszukiwanie.
  Inne, równie dobre,
  rozwiązania mogą przyporządkowywać zadania do maszyn w inny
  sposób, ale nie osiągną mniejszego kosztu, gdyż nie da się zmieścić,
  w czasie dealine'u *D*, na mniejszej liczbie maszyn lub w mniejszej
  liczbie godzin.

- Jeśli obecne rozwiązanie ma koszt większy niż najlepsze dotychczas
  znalezione rozwiązanie, to przerwij przeszukiwanie tej gałęzi.


Partition
~~~~~~~~~
Najmniejszym, niepodzielnym zadaniem jakie można określić dla tego
problemu jest określenie zbioru dzieci danego węzła.
Na podstawie powyższych reguł,
takie zadanie określone musi być poprzez informacje o danym węźle,
deadline'ie *D* oraz najlepszym dotychczas znalezionym rozwiązaniu.


Communication
~~~~~~~~~~~~~
Na potrzeby wykonania elementarnego zadania proces roboczy musi wymienić
dwa komunikaty:

- Otrzymać definicję zadania (węzeł, deadline, najlepsze rozwiązanie),
- Odesłać analogiczne definicje wszystkich dzieci otrzymanego węzła.
  Warto zaznaczyć,
  że wielkość komunikatu zwrotnego może drastycznie
  rosnąc wraz z głębokością,
  na której dokonujemy przeszukiwania w drzewie.


Agglomeration
~~~~~~~~~~~~~
Aby zminimalizować ilość wymienianych komunikatów
(nt. pojedynczych węzłów),
możemy określić pewną ilość gałęzi drzewa,
które mają być potraktowane jako osobne zadania.
Innymi słowy,
musimy dokonać przeszukiwania wszerz do pewnego poziomu głębokości,
gwarantującego odpowiednio dużą w stosunku do ilości dostępnych procesów
ilość gałęzi do przeszukania.


Mapping
-------
Mapowanie zadań określonych jako gałęzie do przeszukania wgłąb dokonywane
jest za pomocą schematu farmy zadań (ang. *task farm*).
Gałęzie-zadania generowane są przez proces farmera za pomocą algorytmu
przeszukiwania wszerz.
Farmer następnie rozsyła zadania do procesów roboczych.
Każda gąłąź jest przeszukiwana wgłąb przez proces roboczy,
jej wynik zwracany, a proces roboczy zgłasza się po następne zadanie.

Nierówne obciążenie wynikłe z różnych czasów przeszukiwania różnych gałęzi
drzewa powinno zostać zniwelowane przez zmapowanie odpowiednio większej
ilości zadań na te procesy,
które dostają zadania stosunkowo krótkie.


Przyspieszenie i efektywność
----------------------------
Serie danych określone są dwoma parametrami: 

- *tasks* - ilość zadań do przyporządkowania maszynom,
- *deadline* - czas, w którym obliczenie musi zostać zakończone.


x2 - laptop, 4 rdzenie
~~~~~~~~~~~~~~~~~~~~~~

.. image:: x2-speedup.png

.. image:: x2-eff.png


blade - klaster obliczeniowy AGH
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. image:: blade-speedup.png

.. image:: blade-eff.png

Wyniki pomiarów
~~~~~~~~~~~~~~~

Powyższe wykresy zostały wygenerowane automatycznie za pomocą skryptu
``prepare-new.py`` dostępnego w archiwum z rozwiązaniem zadania oraz
programu Gnuplot.

Linijka zaczyna się od ilości procesów, które zostały użyte do wykonania
obliczeń.
Po niej następują czasy wykonania dla kolejnych uruchomień programu z
danym zestawem parametrów.
Jednostką czasu jest sekunda.

`Laptop x2 <results.x2.txt>`_

`Klaster blade <results.x2.txt>`_

Wnioski
~~~~~~~
Na podstawie analizy powyższych wykresów oraz bezwzględnych czasów
wykonania programu,
nasuwają się następujące wnioski:

1. Decydujący wpływ na trudność problemu ma parametr deadline.
   Niezależnie od rozmiaru drzewa (tj. od ilości zadań do
   przyporządkowania) zaostrzanie deadline'u wydłuża czas przeszukiwania.

   Tłumaczyć można to tym, że dla dużych deadline'ów w drzewie występuje
   stosunkowo wiele równoważnych ze względu na czas rozwiązań.
   Szybko znajdowane jest akceptowalne ze wzg. na deadline rozwiązanie,
   dzięki czemu duże fragmenty drzewa są potem odcinane jako gorsze (a
   przynajmniej nie lepsze) niż najlepsze znalezione rozwiązanie.

2. Przy słabych ograniczeniach deadline'u zrównoleglenie algorytmu nie
   daje dobrych rezultatów.
   Dominujący wpływ na czas wykonania ma sekwencyjna faza alg.
   równoległego,
   tj. generowanie zadań do równoległego wykonania za pomocą
   przeszukiwania wszerz.
   Otrzymywane czasy są niewiele lepsze lub nawet gorsze niż algorytmu
   czysto sekwencyjnego, przeszukującego tylko wgłąb.

   Dla silnych ograniczeń, tj. niskich wartości deadline'u,
   gdy całkowite czasy działania okazują się być znacznie dłuższe niż faza
   sekwencyjna BFS następuje znaczne przyspieszenie znajdowania
   rozwiązania.

   Niestety, wskaźnik efektywności szybko maleje wraz ze wzrostem ilości
   procesów równolegle wykonujących przeszukiwanie.
   Być może lepsze efekty dałoby się uzyskać dopasowując początkową ilość
   zadań-gałęzi do przeszukania równoległego.

3. Problemy dekomponowane funkcjonalnie wydają się być trudniejsze
   w efektywnej implementacji równoległej z powodu bardziej złożonego
   procesu kodowania zadań do wykonania zdalnego (serializacji złożonych
   struktur a nie tylko pakietów danych liczbowych)
   i konieczności zastosowania skuteczniejszych metod mapowania w celu
   zrównoważenia obciążenia.
