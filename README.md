# clj-conveyor

`[clj-conveyor "1.0.2"]`

Данная библиотека решает задачи организации виртуального стека на ClojureScript (в будущем - Clojure).

## История создания

clj-conveyor является реинкарнацией моей старой библиотеки Conveyor для ActionScript 2001 года. Conveyor на Flash был очень популярен у разработчиков - особенно в русскоязычном Flash-сообществе. Он позволял создавать отложенные во времени вычисления, очереди и анимацию на основе таких очередей. В отличии от других queue-решений, данный подход давал вложенность. То есть, если вызов из очереди, генерирует новые элементы очереди, то они помещаются в точку вызова - некий аналог стэка вызовов, распределенный во времени. Его так и называли "Конвейер Потапенко".

## Новая версия

Так как я пишу на clojure последнее время, мне понадобился и Conveyor. 
Функционал конвейера улучшен с помощью каналов core.async, а так же удален код прямого доступа к очереди (добавление в конец, начало очереди) - сейчас я считаю такой подход избыточным и даже  вредным. Только "callstack". 


## Очередь

Просто  очередь, ничего сложного. Передаем функцию, время задержки (опционально) и список аргументов для функции.

```
(:require [clj-conveyor.core :refer [conv add]])

(-> conv (add #(println "hello conv 1") 500 [])) ;; вывести сообщение и ждать 500ms
(-> conv (add #(println "hello conv 2") 500 [])) ;; вывести сообщение и ждать 500ms
(-> conv (add #(println "hello conv 3") 500 [])) ;; вывести сообщение и ждать 500ms

(-> conv (add #(println "hello conv" %) 500 [4])) ;; вывести сообщение, передать аргумент, и ждать 500ms
(-> conv (add #(println "hello conv" %) 500 [5])) ;; вывести сообщение, передать аргумент, и ждать 500ms
(-> conv (add #(println "hello conv" %) 500 [6])) ;; вывести сообщение, передать аргумент, и ждать 500ms
```

Результат:

```
hello conv 1
hello conv 2
hello conv 3
hello conv 4
hello conv 5
hello conv 6
```

## Вложенная очередь - "стэк вызовов"

Вложенность - это когда вызовы из очереди, добавляющие элементы очередь, помещают новые команды в точку вызова.
 Другими словами каждый элемент очереди может сам быть очередью.

![Конвейер (спасибо за иллюстрацию @fmnoise)](https://raw.githubusercontent.com/potapenko/clj-conveyor/master/image@2x.png)

_(спасибо за иллюстрацию [@fmnoise](https://github.com/fmnoise))_


```
(-> conv (add (fn []
             (-> conv (add #(println "hello conv 1") 500 []))
             (-> conv (add (fn []
                             (-> conv (add (fn []
                                             (-> conv (add #(println "hello conv 2") 500 []))
                                             (-> conv (add #(println "hello conv 3") 500 [])))
                                           500 [3]))
                             (-> conv (add #(println "hello conv 4") 500 []))
                             (-> conv (add #(println "hello conv 5") 500 [])))
                           500 []))
             (-> conv (add #(println "hello conv 6") 500 []))
             (-> conv (add #(println "hello conv 7") 500 [])))))
```
Результат:

```
hello conv 1
hello conv 2
hello conv 3
hello conv 4
hello conv 5
hello conv 6
hello conv 7
```

Какая бы вложенность вызовов из функция не была, мы получаем правильный порядок выполнения.

Подход полезен при отложенных вычислениях, когда мы создаем несколько элементов очереди, но не имеем достаточно данных для вычисления в этот момент. Например, мы создаем анимацию перемещения графического элемента, и при создании очереди, мы не знаем координаты графического элемента в будущем (например, он может переместиться другими командами). Чтобы решить эту задачу мы добавим вызов "создать анимацию для графического элемента" в очередь (отложим это вычисление), и когда он выполнится в будущем, будут добавлены элементы очереди, создающее анимацию перемещения. Важно что что анимация будет выполнена сразу после выполнения элемента "создать анимацию".


## Несколько экземпляров конвейеров

По умолчанию мы имеем глобальный конвейер для всего приложения. Чаще всего его достаточно. Но в некоторых случаях необходимо запустить несколько "потоков". Для этого нужно создать конвейер с помощью функции-фабрики `->conv`

```
(:require [clj-conveyor.core :refer [conv add ->conv]])

(def local-conveyor (->conv))

(-> local-conveyor (add #(println "hello from local conveyor")));
```


## Другие методы

`start/stop` -  остановить конвейер

`stoped?` - проверить остановлен ли

`clear` - очистить очередь 

`pause` - остановить конвейер на время - например, вы используете библиотеку для анимации и хотели бы подождать пока отрисуется анимация, а потом продолжить поток исполнения. Аналогичен вызову `(-> conv (add #() pause-time-in-ms)`

`clear-and-stop` - остановить и очистить. 

```
(defprotocol IConveyor
  (init [this])
  (add [this cb] [this cb args] [this cb t args])
  (pause [this t])
  (start [this])
  (stop [this])
  (stoped? [this])
  (clear [this])
  (clear-and-stop [this]))
```

## License

Copyright © 2017 Eugene Potapenko

Distributed under the Eclipse Public License, the same as Clojure.
