# Ein neoklassisches Malthusianisches Wachstumsmodell

Diese App implementiert eine gleichgewichtsbasierte Interpretation von
Malthus' Wachstumsmodell.

Eine genaue Beschreibung des Modells und der Implementierung findet sich in
`beschreibung/Malthus_Modellbeschreibung.pdf`.

Mögliche Leitfragen für die Experimente mit dem Modell:

1. Was impliziert eine Veränderung in der Sterberate, z.B. durch einen Virus?"
2. Was ist der Effekt eines Anstiegs der Geburtenrate?
3. Wie zeigt sich eine Änderung der TFP durch eine technische Innovation?
4. Welche Rolle spielt der Ausgangswert für die Bevölkerungsgröße? Warum?

## Technische Hinweise zur Verwendung der App

Um die App zu verwenden gehen Sie folgendermaßen vor:

1. Laden Sie sich die Repository herunter
2. Öffnen Sie das entsprechende R Projektfile `malthus_gleichgewichtsbasiert.Rproj`
3. Öffnen Sie das file `app.R`
4. Klicken Sie oben rechts aus `Run App` (ich empfehle Ihnen dabei über das Drop-Down Menü `Run external` auszuwählen, damit die App im Browser geöffnet wird)

Alternativ können Sie auch den Link im Moodle verwenden, allerdings ist die 
Nutzungszeit hier pro Monat für den Kurs beschränkt und Sie können sich nicht 
den Code ansehen.

Eine dritte Möglichkeit besteht darin, die App lokal zu installieren und über
ihre lokale R-Version zu benutzen. 
Führen Sie dazu folgenden Code aus:

```
shiny::runGitHub(username = "graebnerc", repo = "malthus_neoklassik")
```

## Hinweise zu notwendigen Paketen und eventuellen Updates

Wenn Sie die App auf Ihrem Computer verwenden wollen müssen Sie bestimmte 
Pakete installiert haben. Zudem sollte Ihre R Version nicht zu alt sein.
Sie können das automatisiert mit dem Skritpt `versionstest.R` überprüfen. 
Hier werden eventuell fehlende Pakete automatisch installiert. 
Zudem bekommen Sie einen Hinweis wenn Sie R oder ein Paket updaten sollten.

Wenn Sie R auf Windows updaten müssen gehen Sie folgendermaßen vor:

1. Installieren Sie das Paket `installr`: `install.packages("installr")`
2. Führen Sie folgenden Code aus:

```
library(installr)
updateR()
```

3. Checken Sie ob R-Studio geupdated werden muss indem Sie unter `Hilfe` nach neuen Versionen suchen.

Wenn Sie R auf dem Mac updaten wollen, installieren Sie R einfach neu.



Für eventuelle Fragen melden Sie sich bitte bei Claudius Gräbner
