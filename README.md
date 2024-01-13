### **Wordle-like game in Haskell**
#### Това е проект за курса по Функционално програмиране във ФМИ
##### Автор на проекта: Дончо Иванов
##### ФН: 1MI0800211
##### Условието на проекта може да бъде намерено на: https://docs.google.com/document/d/1mejFKpe52EpmlNa7JkntO1r8YBa7EaIcZG2RXf_--os/edit

* **Идеята зад решението**
	>Проектът създава клонинг на Wordle. Не бих го нарекъл истински Wordle, защото има малко различия. Има няколко различни игрови режима и нива на трудност. Още от началото проекта го започнах като CLI програма. Всички функции са направени така, че да симулират колкото се може по-добре истинската Wordle игра. Всеки игрови режим и ниво на трудност има максимален брой опити - 6. Това число е hard coded в кода, защото не може да бъде променяно и не мисля, че има смисъл да е извадено като 'константа'. Играта се играе като се въвеждат команди в конзолата (режим REPL). В ситуации в които играчът въведе команда, която предизвиква проблем по време на игра (пример: невалидна дължина на думата), съм избрал да терминирам действието на програмата, за да не може да бъде продължена играта с грешен input. Имах и за цел да направя user-interface-а на играта (ако може да бъде наречен такъв) поне малко приятен за очите и удобен за играча. Всички нива на трудност от "guess" игровия режим позволяват игрането на думи, които не са в речника. Лесното ниво на трудност предупреждава ако бъде направено това.

* **Структура на проекта**
	>Проектът е създаден с Cabal build системата.
	>Проектът е разделен на 4 файла
	>	==Main.hs== : Този файл е главната точка за достъп до проекта. В него е логиката за работа с менюто. В зависимост от това какво въведе играчът, ==main== функцията вика съответната функция на избрания игрови режим. Всичко това е постигнато с ==getLine== функцията и разглеждане на различни случаи за подадения вход с ==case of==. Ако играчът въведе невалиден вход, то той бива предупреден и ==main== функцията се вика наново.
	>
	>	==GuessMode.hs==: Това е най-големият файл в проекта. В него се съдържа логиката за "guess" игровия режим. Най-отгоре на файла има дефиниран data тип ==Square==. Този data тип представлява "квадратчетата", които отговарят на различните букви от въведената дума. Има дефинирани инстанции на ==Show==, за да може квадратите да се виждат като емоджита на конзолата (понеже работя на UNIX-базирана ОС, не гарантирам, че тези емоджита ще се появяват на друг тип ОС, пр.: Windows), и на ==Read==, за да може квадратите да бъдат прочитани от конзолата. Функцията ==compareWords== е, може би, най-важната функция в проекта. Тя работи използвайки ==compareWordsHelper==. Функцията приема два стринга и връща квадратчетата, които отговарят на първия стринг, спрямо втория.
	>	`g u e s s 
	>	⬜⬜🟨🟩`
	>	==playGuessModeNormal== функцията реализира "нормалното" ниво на трудност. Приема число - на кой ход е играчът и думата, която трябва да познае. Ако въведената от играча дума съвпада с тази, която трябва да познае то ходът на функцията приключва, иначе бива викана ==compareWords== функцията на думата, която трябва да бъде позната и на тази въведена от играчът, принтира се резултатът и функцията бива извикана наново с turn + 1.
	>	==playGuessModeEasy== функцията реализира "лесното" ниво на трудност. Функцията приема Map Char (Int, Square), който служи за това да се следят буквите, които са били познати досега и какъв квадрат им отговаря на съответната позиция. Приема речникът с думи с дължина n, както и отново ходът на който е играчът и думата, която трябва да познае. Реализирани са проверки за предупрежденията изисквани от условието използвайки обхождане над Map-а. Този Map се попълва след всеки ход, използвайки помощната функция ==handleLettersMap==.
	>	==playGuessModeExpert== функцията реализира "експертното" ниво на трудност. Функцията приема число, което е номерът на рунда, в който програмата трябва да излъже играча (този номер е произволно генериран), Map, който бъде пълнен по същия начин като в ==playGuessModeEasy== функцията, използвайки ==handleLettersMap==. Този Map се използва, за да може лъжата, която се създава да не противоречи с предишните отговори. Както и останалите, функцията приема и ходът на който е играчът и думата, която трябва да бъде позната. Функцията използва помощната ==createLie==, за да създаде лъжливия отговор, когато е време да лъже.
	>
	>	==HelpMode.hs==: В този файл е реализирана логиката за игровия режим "help". Има само една функция - ==playHelpMode==. Тази функция приема речник с думи, ход и думата, която трябва да бъде позната от компютъра. При всеки ход, компютърът избира дума от речника и играчът трябва да въведе квадратите, които отговарят на тази дума, спрямо думата, която трябва да бъде позната (напр.: "Gray Gray Yellow Green Yellow"). След това речникът бива филтриран да съдържа само думи, чиито ==compareWords== (import-ната от ==GuessMode.hs==) резултат с думата, която е избрал компютърът, отговаря на квадратите въведени от потребителят. Това което се различава с условието в тази функцият е, че не съм имплементирал третата стъпка на алгоритъма даден в условието за избиране на дума от компютъра.
	>
	>	==GenerateWord.hs==: Този файл съдържа логика за четене на данни от файл с думи и генериране на дума, която трябва да бъде позната.
	>	==filterWordList== функцията приема число n - дължина на думи. Функцията чете файла ==words_alpha.txt.== (Взет от https://github.com/dwyl/english-words, под Unlicense license лиценза) след това го филтрира, връщайки само думите от него с дължина n.
	>	==generateWord== приема число n - дължина на дума и избира произволна дума от "речника" (който съдържа само думи с дължина n).

* **Работа с проекта**
	>Проектът се run-ва използвайки командата ==cabal run==. Отдолу е кратък пример за работа с проекта - изиграна игра на лесното ниво на трудност на "guess" режима.

`% Choose a game mode: 'guess' or 'help' or type 'quit' to quit:`
`guess`
`% Choose a level of difficulty: 'easy', 'normal' or 'expert' or 'back' to go back to mode selection`
`easy`
`% Input a word length:`
`5`
-------------------------------

`% Enter your word guess:`
`fjord`
-------------------------------
`f j o r d`
`⬜⬜🟨🟨⬜`

`% Enter your word guess:`
`nymph`
-------------------------------
- `The following letters are known to be yellow, but are not in your guess: 'o' 'r'`
`n y m p h`
`⬜⬜⬜🟨⬜`

`% Enter your word guess:`
`roped`
-------------------------------
- `The following letters are known to be gray, but are in your guess: 'd'`
`r o p e d`
`🟨🟩🟨⬜⬜`

`% Enter your word guess:`
`porch`
-------------------------------
- `The following letters are known to be gray, but are in your guess: 'h'`
`p o r c h`
`🟩🟩🟩⬜⬜`

`% Enter your word guess:`
`porta`
-------------------------------
`p o r t a`
`🟩🟩🟩🟩⬜`

`% Enter your word guess:`
`ports`
-------------------------------
`p o r t s`
`🟩🟩🟩🟩🟩`
`Correct guess!`