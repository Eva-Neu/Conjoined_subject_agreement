PennController.ResetPrefix(null)


SetCounter("counter");
Sequence("counter", "welcome", "induction", "examples", "ready", randomize("critical"), "dialect",  "feedback", "captcha", SendResults(), "goodbye");

newTrial("welcome",
newText("welcome1-text", "<p>Einverständniserklärung</p>")
    .center()
    .print()
    ,
     newText("welcome2-text", "<p>Sie sind eingeladen, an einer Studie mit dem Titel Sentence acceptability judgment study for German speakers teilzunehmen. Diese Studie wird von Eva Neu, M.A., und Dr. Brian Dillon von der University of Massachusetts Amherst durchgeführt. Sie wurden zur Teilnahme an dieser Studie ausgewählt, weil Sie deutsche/-r Muttersprachler/-in sind.</p> ")
    .center()
    .print()
    ,
    newText("welcome3-text", "<p>Warum führen wir diese Studie durch?</p><p>Das Ziel dieser Studie ist es, bestimmte grammatikalische Phänomene des Deutschen zu untersuchen. Zu diesem Zweck werden wir Ihnen Sätze auf einem Bildschirm zeigen und Sie fragen, wie natürlich Ihnen diese Sätze erscheinen.</p> ")
    .center()
    .print()
    ,
    newText("welcome4-text", "<p>Wer kann an dieser Studie teilnehmen?</p><p>Um an dieser Studie teilnehmen zu können, müssen Sie einsprachige/-r deutsche/-r Muttersprachler/-in sein und in einem deutschsprachigen Land wohnen. Sie dürfen keine Sprachstörung, Lese- und Rechtschreibstörung oder andere Einschränkung haben, die Ihre Fähigkeit beeinträchtigt, deutsche Sätze zu lesen und zu verstehen. Sie müssen mindestens 18 Jahre alt sein.</p>")
    .center()
    .print()
    ,
    newText("welcome5-text", "<p>Was wird von mir verlangt und wie viel Zeit werde ich dafür benötigen?</p><p>Wenn Sie sich bereit erklären, an dieser Studie teilzunehmen, werden Sie aufgefordert, Sätze auf einem Bildschirm zu lesen. Nachdem Sie jeweils einen Satz gelesen haben, werden Sie gebeten, ihn zu bewerten, indem Sie auf eine Schaltfläche auf dem Bildschirm klicken. Zu Beginn des Experiments erhalten Sie Anweisungen, wie Sie die Sätze bewerten sollen. Wir werden Ihre Antworten aufzeichnen. Im Rahmen des Experiments werden Ihnen einige kurze Fragen zu Ihren Gedanken zu dem Experiment und Ihrem sprachlichen Hintergrund gestellt.</p><p>Das gesamte Experiment sollte von Anfang bis Ende 20 bis 30 Minuten dauern. Sie werden in Zukunft nicht mehr kontaktiert werden.</p>")
    .center()
    .print()
    ,
    newText("welcome6-text", "<p>Wird mir die Teilnahme an dieser Studie in irgendeiner Weise nützen?</p><p>Sie werden keinen direkten Nutzen aus dieser Studie ziehen; wir hoffen jedoch, dass Ihre Teilnahme an der Studie uns helfen wird, die Grammatik und den Gebrauch der menschlichen Sprache besser zu verstehen.</p>")
    .center()
    .print()
    ,
    newText("welcome7-text", "<p>Welche Risiken bestehen für mich bei der Teilnahme an dieser Studie?</p><p>Es besteht ein minimales Risiko, dass Sie sich durch längeres Sitzen unwohl fühlen. Es bestehen nur minimale Risiken für Ihr soziales und psychisches Wohlbefinden, da das gesamte Sprachmaterial so bearbeitet wurde, dass Inhalte, die Ihnen Unbehagen bereiten könnten, entfernt wurden. Es besteht kein Risiko für Ihr wirtschaftliches Wohlergehen. Bei Online-Studien besteht immer das Risiko einer Verletzung der Vertraulichkeit. Wir haben jedoch Maßnahmen ergriffen, um dieses Risiko zu minimieren, die wir im Folgenden beschreiben.</p>")
    .center()
    .print()
    ,
    newText("welcome8-text", "<p><Wie werden meine persönlichen Daten geschützt?</p><p>Die folgenden Maßnahmen werden eingesetzt, um die Vertraulichkeit Ihrer Studiendaten zu schützen. Erstens werden wir keine Verbindung zwischen Ihren Daten und Ihnen als Person herstellen. Dadurch wird das Risiko beseitigt, dass Ihre Antworten und Daten mit Ihnen als Person in Verbindung gebracht werden können. Nach Abschluss der Studie können die Forscher ihre Ergebnisse veröffentlichen. Die Informationen werden in zusammengefasster Form dargestellt, und Sie werden in den Publikationen oder Präsentationen nicht genannt. Die anonymisierten Daten können jedoch im Rahmen dieser Publikationen veröffentlicht werden.</p>")
    .center()
    .print()
    ,
    newText("welcome9-text", "<p>Bekomme ich Geld oder eine andere Entschädigung für die Teilnahme an dieser Studie?</p><p>Wenn Sie die Studie vollständig abschließen, werden Sie über Prolific mit 8 Dollar entschädigt.</p>")
    .center()
    .print()
    ,
    newText("welcome10-text", "<p>Was passiert, wenn ich ja sage, aber später meine Meinung ändere?</p><p>Sie müssen nicht an dieser Studie teilnehmen, wenn Sie nicht wollen. Wenn Sie zustimmen, an der Studie teilzunehmen, es sich aber später anders überlegen, können Sie jederzeit aussteigen. Es gibt keinerlei Strafen oder Konsequenzen, wenn Sie sich gegen eine Teilnahme entscheiden.</p>")
    .center()
    .print()
    ,
    newText("welcome11-text", "<p>An wen kann ich mich wenden, wenn ich Fragen habe?</p><p>Wir beantworten gerne alle Ihre Fragen zu dieser Studie. Wenn Sie weitere Fragen zu diesem Projekt haben oder wenn Sie ein forschungsbezogenes Problem haben, können Sie sich an die Studienleiterin Eva Neu unter eneu@umass.edu wenden. Wenn Sie mit jemandem sprechen möchten, der nicht direkt an der Studie beteiligt ist, können Sie sich an den Leiter des Fachbereichs Linguistik, Dr. Joe Pater, unter pater@linguist.umass.edu oder telefonisch unter +1 (413) 577-1308 wenden, oder an das Human Research Protection Office der University of Massachusetts per E-Mail unter humansubjects@ora.umass.edu, telefonisch unter +1 (413) 545-3428 oder per Post an das Human Research Protection Office, Mass Venture Center, 100 Venture Way, Suite 116, Hadley MA, 01035, USA.</p><p>Indem Sie unten auf 'Ich stimme zu' klicken, geben Sie an, dass Sie mindestens 18 Jahre alt sind, diese Einverständniserklärung gelesen haben und mit der Teilnahme an dieser Studie einverstanden sind.</p><p>Bitte drucken Sie eine Kopie dieser Seite für Ihre Unterlagen aus.</p>")
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
newTrial("induction",
     newText("induction-text", "<p>Ihre Aufgabe in diesem Experiment ist es, Sätze auf einer Skala von 1 bis 5 daraufhin zu bewerten, wie grammatikalisch akzeptabel sie sind. Wichtig ist dabei nicht, ob ein Satz den offiziellen Grammatikregeln gehorcht, die Sie vielleicht in der Schule gelernt haben, sondern ob er für Sie spontan natürlich, normal und richtig klingt und Sich sich vorstellen können, dass ihn reale Sprecherinnen und Sprecher im Gespräch verwenden würden. Auf der nächsten Seite werden Ihnen einige Beispiele gezeigt. </p> ")
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
    
newTrial("examples",
    newText("examples1", "<p> Hier ist ein Beispiel für einen völlig grammatischen und natürlichen Satz: </p> <p> Linda versprach ihrem Sohn, ihn von der Schule abzuholen.</p>")
    .print()
    ,
    newText("examples2", "<p> Hier ist ein Beispiel für einen völlig ungrammatischen Satz: </p> <p> Die Kinder reichten Geschenk das Lehrerin über.</p>")
    .print()
    ,
    newText("examples3", "<p> Hier ist ein Beispiel für einen Satz auf der Mitte der Skala: </p> <p> Der Roman wurde sich schon von drei verschiedenen Leuten ausgeliehen. </p>")
    .print()
    ,
    newText("<p> Bitte lesen Sie sich die Sätze, die Ihnen während des Experiments angezeigt werden, sorgfältig durch und vergeben Sie fünf Punkte für völlig grammatische, einen Punkt für völlig ungrammatische Sätze. </p>")
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
newTrial("ready",
     newText("data-text", "<p> Das Experiment wird ungefähr 20 bis 30 Minuten dauern. Wir empfehlen, währenddessen zwei oder drei kurze Pausen einzulegen. </p> <p>  Wenn Sie bereit sind, klicken Sie bitte nun auf die Schaltfläche unten, um zu beginnen. </p>")
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
    newText("Bitte geben Sie im untenstehenden Textfeld an, ob Sie einen Dialekt sprechen, und wenn ja, welchen. Diese Angabe ist freiwillig.")
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
    newText("Bitte nutzen Sie das untenstehende Textfeld für eventuelle Fragen, Anmerkungen oder Gedanken zum Experiment. Ist Ihnen etwas Bestimmtes aufgefallen? Haben Sie eine spezielle Strategie angewandt? Hatten Sie den Eindruck, dass manche Formen nur in manchen Kontexten akzeptable sind?")
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
newTrial("captcha",
    newText("Bitte geben Sie drei konkrete Beispiele für Dinge, die man typischerweise in einer Bäckerei erwirbt. Seien Sie dabei bitte spezifisch.")
    .center()
    .print()
    ,
    newText(" ")
    .center()
    .print()
    ,
    newTextInput("captcha-input")
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
    newText("goodbye-text", "<p> Das war's! Vielen Dank für Ihre Teilnahme. <a href='https://app.prolific.co/submissions/complete?cc=51196653"+GetURLParameter("id")+"' target='_blank'>Bitte klicken Sie nun auf diesen Link, der Sie zurück zu Prolific bringt.</a> Dieser Schritt ist notwendig, damit Sie für Ihre Teilnahme entlohnt werden können!</p>")
    .center()
    .print()
    ,
    newButton("void")
    .wait()
    )



