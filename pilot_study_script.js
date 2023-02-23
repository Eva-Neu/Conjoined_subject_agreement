PennController.ResetPrefix(null)

Header(
)
.log("PROLIFIC_ID" , GetURLParameter("id"))

SetCounter("counter");
Sequence("counter", "welcome", "intro", "data", "ready", randomize("critical"), "dialect",  "feedback", SendResults(), "goodbye");

newTrial("welcome",
     newText("welcome-text", "<p> Willkommen! Deine Aufgabe in diesem Experiment ist es, Sätze auf einer Skala von 1 bis 5 daraufhin zu bewerten, wie grammatikalisch akzeptabel sie sind. Wichtig ist dabei nicht, ob ein Satz den offiziellen Grammatikregeln gehorcht, die du vielleicht in der Schule gelernt hast, sondern ob er für dich spontan natürlich, normal und richtig klingt und du dir vorstellen kannst, dass ihn reale Sprecherinnen und Sprecher im Gespräch verwenden würden. Auf der nächsten Seite werden dir einige Beispiele gezeigt. </p> ")
    .center()
    .print()
    ,
    newText("<p></p>")
    .center()
    .print()
    ,
    newButton("Weiter")
    .center()
    .settings.size(200, 30)
    .print()
    .wait()
    )
    ,
    
newTrial("intro",
    newText("intro1", "<p> Hier ist ein Beispiel für einen völlig grammatischen und natürlichen Satz: </p> <p> Linda versprach ihrem Sohn, ihn von der Schule abzuholen.</p>")
    .print()
    ,
    newText("intro2", "<p> Hier ist ein Beispiel für einen völlig ungrammatischen Satz: </p> <p> Die Kinder reichten Geschenk das Lehrerin über.</p>")
    .print()
    ,
    newText("intro3", "<p> Hier ist ein Beispiel für einen Satz auf der Mitte der Skala: </p> <p> Der Roman wurde sich schon von drei verschiedenen Leuten ausgeliehen. </p>")
    .print()
    ,
    newText("<p> Bitte vergib fünf Punkte für völlig grammatische, einen Punkt für völlig ungrammatische Sätze. </p>")
    .center()
    .print()
    ,
    newText("<p></p>")
    .center()
    .print()
    ,
    newButton("Weiter")
    .center()
    .settings.size(200, 30)
    .print()
    .wait()
    )
    ,
    
newTrial("data",
     newText("data-text", "<p> data privacy - TBD </p>")
    .center()
    .print()
    ,
    newText("<p></p>")
    .center()
    .print()
    ,
    newButton("Ich stimme zu")
    .center()
    .settings.size(200, 30)
    .print()
    .wait()
    )
    ,
    
newTrial("ready",
     newText("data-text", "<p> Das Experiment wird ungefähr 20 bis 30 Minuten dauern. </p> <p>  Bitte klicke nun auf die Schaltfläche unten, um zu beginnen. </p>")
    .center()
    .print()
    ,
    newText("<p></p>")
    .center()
    .print()
    ,
    newButton("Ich bin bereit!")
    .center()
    .settings.size(200, 30)
    .print()
    .wait()
    )
    ,
    
Template("items.csv", variable =>

    newTrial("critical",
        newText("sent", variable.Sentence)
            .center()
            .print()
        ,
        newText(" ")
            .print()
        ,  
        newScale("rating","1", "2", "3", "4", "5")
        .button()
        .before( newText("left", "Völlig ungrammatisch") )
        .after( newText("right", "Völlig grammatisch") )
        .log()
        .center()
        .labelsPosition("bottom")
        .print()
        .wait()
        )
        .log( "Sentence", variable.Sentence)
        .log( "Condition", variable.Condition)
        .log( "Item_set", variable.Item_set)
        .log( "Correct", variable.Correct)
        .log( "Group", variable.Group)

)

newTrial("dialect",
    newText("Bitte gebe im untenstehenden Textfeld an, ob du einen Dialekt sprichst, und wenn ja, welchen. Diese Angabe ist freiwillig.")
    .center()
    .print()
    ,
    newText(" ")
    .center()
    .print()
    ,
    newTextInput("dialect-input")
    .center()
    .lines(0)
    .log()
    .size(400, 200)
    .print()
    ,
    newText("<p></p>")
    .center()
    .print()
    ,
    newButton("Abschicken")
    .center()
    .settings.size(200, 30)
    .print()
    .wait()
    )
    ,
newTrial("feedback",
    newText("Bitte nutze das untenstehende Textfeld für eventuelle Fragen, Anmerkungen oder Gedanken zum Experiment. Ist dir etwas Bestimmtes aufgefallen? Hast du eine spezielle Strategie angewendet? Hattest du den Eindruck, dass manche Formen nur in manchen Kontexten akzeptable sind?")
    .center()
    .print()
    ,
    newText(" ")
    .center()
    .print()
    ,
    newTextInput("feedback-input")
    .center()
    .log()
    .lines(0)
    .size(400, 200)
    .print()
    ,
    newText("<p></p>")
    .center()
    .print()
    ,
    newButton("Abschicken")
    .center()
    .settings.size(200, 30)
    .print()
    .wait()
    )
    ,
newTrial("goodbye",
    newText("goodbye-text", "<p> Das war's! Vielen Dank für deine Teilnahme. <a href='https://app.prolific.co/submissions/complete?cc=51196653"+GetURLParameter("id")+"' target='_blank'>Bitte klicke nun auf diesen Link, der dich zurück zu Prolific bringt.</a> Dieser Schritt ist notwendig, damit du für deine Teilnahme entlohnt werden kannst!</p>")
    .center()
    .print()
    ,
    newButton("void")
    .wait()
    )




