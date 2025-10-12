# Generate a cleaner bilingual (ES–EN) questionnaire report using longtable with two columns
# Usage:
#   R -e "source('github/education/scripts/export_student_questions_bilingual_clean.R'); export_student_questions_bilingual_clean()"

suppressPackageStartupMessages({
  library(stringr)
  library(tibble)
  library(dplyr)
})

# Resolve paths
get_script_dir <- function() {
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("--file=", "", file_arg[1]))))
  normalizePath(getwd(), mustWork = TRUE)
}

SCRIPT_DIR <- get_script_dir()
ROOT_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = TRUE)
REPORTS_DIR <- file.path(ROOT_DIR, "reports")
if (!dir.exists(REPORTS_DIR)) dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Reuse helpers by sourcing the existing exporter (for dictionary parsing and translation)
source(file.path(SCRIPT_DIR, "export_student_questions_en.R"))

# Latex escaper
tex_esc <- function(s) {
  s <- as.character(s)
  s[is.na(s)] <- ""
  s <- gsub("\\\\", "\\\\textbackslash{}", s)
  s <- gsub("([#%&_$])", "\\\\\\1", s)
  s <- gsub("~", "\\\\textasciitilde{}", s)
  s <- gsub("\\^", "\\\\textasciicircum{}", s)
  s <- gsub("{", "\\\\{", s, fixed = TRUE)
  s <- gsub("}", "\\\\}", s, fixed = TRUE)
  s
}

# Local ES->EN translator (simple rule-based, preserves codes like "p10_01: " and numeric prefixes "1: ")
translate_local_vec <- function(x) {
  if (length(x) == 0) return(x)
  x <- as.character(x)
  x[is.na(x)] <- ""
  out <- character(length(x))
  # helper: split prefix code/number from rest
  split_prefix <- function(s) {
    m <- regexpr("^(?:p[0-9]{2}(?:_[0-9]{2})?: |[0-9]+:\\s*)", s, perl = TRUE)
    if (m[1] > 0) {
      pre <- substr(s, m[1], m[1] + attr(m, 'match.length') - 1)
      rest <- substr(s, m[1] + attr(m, 'match.length'), nchar(s))
      list(pre = pre, rest = trimws(rest))
    } else list(pre = "", rest = trimws(s))
  }
  # exact questions
  dict_exact <- c(
    "¿Cuántos años tienes?" = "How old are you?",
    "¿Cuál fue la primera lengua que aprendiste a hablar?" = "What was the first language you learned to speak?",
    "¿Qué lengua hablan en tu casa la mayor parte del tiempo?" = "What language is mostly spoken at home?",
    "Durante el último mes, ¿cuántos días has faltado a la escuela?" = "In the last month, how many days did you miss school?",
    "¿Qué tan de acuerdo estás con las siguientes afirmaciones?" = "How much do you agree with the following statements?",
    "¿Qué tan preparado te sientes para comenzar el 1.° grado de secundaria el siguiente año?" = "How prepared do you feel to start 1st year of secondary school next year?",
    "¿Cuál crees que será el nivel de estudios más alto que alcanzarás?" = "What is the highest level of education you think you will achieve?",
    "En una semana habitual, ¿cuántas horas les dedicas a la lectura en tu TIEMPO LIBRE?" = "In a typical week, how many hours do you spend reading in your FREE TIME?",
    "¿Qué tan de acuerdo estás con los siguientes enunciados sobre las razones por las que lees en tu TIEMPO LIBRE?" = "How much do you agree with the following statements about why you read in your FREE TIME?",
    "Cuando estudias un tema para aprender o para dar un examen, ¿qué tan seguido realizas las siguientes acciones?" = "When you study a topic to learn or for a test, how often do you do the following?",
    "¿Qué tan de acuerdo estás con los siguientes enunciados sobre tu clase de COMUNICACIÓN?" = "How much do you agree with the following statements about your COMMUNICATION class?",
    "¿Qué tan seguido el profesor que enseña COMUNICACIÓN realiza las siguientes acciones?" = "How often does the teacher who teaches COMMUNICATION do the following?",
    "¿Qué tan seguido el profesor que enseña MATEMÁTICA realiza las siguientes acciones?" = "How often does the teacher who teaches MATHEMATICS do the following?",
    "¿Qué tan seguido ocurren las siguientes situaciones en las clases de MATEMÁTICA?" = "How often do the following situations occur in MATH classes?",
    "¿Qué tan de acuerdo estás con las siguientes afirmaciones relacionadas con la matemática?" = "How much do you agree with the following statements related to mathematics?",
    "¿Qué tan seguido el profesor o profesora que enseña PERSONAL SOCIAL hace lo siguiente?" = "How often does the SOCIAL PERSONAL teacher do the following?",
    "¿Qué tan de acuerdo estás con las siguientes afirmaciones?" = "How much do you agree with the following statements?",
    "¿Qué tan de acuerdo estás con las siguientes afirmaciones sobre tu participación en las clases?" = "How much do you agree with the following statements about your participation in class?",
    "Durante las clases, ¿qué tan seguido tus profesores hacen lo siguiente?" = "During classes, how often do your teachers do the following?",
    "Con respecto a tu experiencia escolar este año, ¿con qué frecuencia te sientes de la siguiente manera?" = "Regarding your school experience this year, how often do you feel the following way?",
    "Durante este año, ¿con qué frecuencia un PROFESOR(A) U OTRO ADULTO de tu colegio ha actuado de la siguiente manera? Piensa en situaciones que incluyan contacto cara a cara, mensajes de texto, WhatsApp, redes sociales o por otro medio por internet." = "During this year, how often has a TEACHER OR ANOTHER ADULT at your school acted in the following way? Think of situations including face-to-face contact, text messages, WhatsApp, social media, or other internet means.",
    "Durante este año, ¿con qué frecuencia ALGUNOS COMPAÑEROS(AS) han actuado de la siguiente manera con otros(as) estudiantes? Piensa en situaciones que incluyan contacto cara a cara, mensajes de texto, WhatsApp, redes sociales o por otro medio por internet." = "During this year, how often have SOME CLASSMATES acted in the following way toward other students? Think of situations including face-to-face contact, text messages, WhatsApp, social media, or other internet means.",
    "Cuando las situaciones mencionadas en las preguntas anteriores han sucedido en tu colegio, ¿le contaste y/o solicitaste ayuda a las siguientes personas?" = "When the situations mentioned in the previous questions happened at your school, did you tell and/or ask the following people for help?",
    # P05 sub-items
    "Me emociona la idea de comenzar a estudiar la secundaria" = "I am excited about starting secondary school",
    "Me da miedo comenzar la secundaria" = "I am afraid to start secondary school",
    "Creo que mis notas serán mejores en la secundaria" = "I think my grades will be better in secondary school",
    "Me gustaría que la escuela acabe en 6.° grado de primaria y no tener que estudiar más" = "I would like school to end in 6th grade of primary and not have to study further",
    "Preferiría ponerme a trabajar en lugar de entrar a la secundaria" = "I would rather start working instead of entering secondary school",
    "Siento que en la secundaria será muy difícil aprobar todos los cursos" = "I feel it will be very hard to pass all courses in secondary school",
    "Me preocupa no tener el dinero suficiente para seguir estudiando" = "I worry about not having enough money to keep studying",
    "Me preocupa sufrir de violencia o acoso escolar (bullying) en secundaria" = "I worry about suffering violence or school bullying in secondary school"
  )
  dict_terms <- c(
    # Likert
    "Totalmente en desacuerdo" = "Strongly disagree",
    "En desacuerdo" = "Disagree",
    "De acuerdo" = "Agree",
    "Totalmente de acuerdo" = "Strongly agree",
    # Frequency
    "Nunca o casi nunca" = "Never or almost never",
    "Algunas veces" = "Sometimes",
    "Muchas veces" = "Often",
    "Siempre o casi siempre" = "Always or almost always",
    "En algunas clases" = "In some classes",
    "En la mayoría de las clases" = "In most classes",
    "En todas o en casi todas las clases" = "In all or almost all classes",
    "Nunca" = "Never",
    "Unas cuantas veces al año" = "A few times a year",
    "Unas cuantas veces al mes" = "A few times a month",
    "Una o más veces por semana" = "Once or more times per week",
    # Binary
    "Sí" = "Yes",
    "No" = "No",
    # Time amounts
    "Menos de 10 años" = "Under 10 years",
    "Más de 12 años" = "Over 12 years",
    "De 0 a 1 día" = "From 0 to 1 day",
    "De 2 a 4 días" = "From 2 to 4 days",
    "De 5 a 7 días" = "From 5 to 7 days",
    # Reading hours
    "No leo en mi tiempo libre" = "I do not read in my free time",
    "Menos de 2 horas a la semana" = "Less than 2 hours per week",
    "Entre 2 y 4 horas a la semana" = "Between 2 and 4 hours per week",
    "Entre 5 y 7 horas a la semana" = "Between 5 and 7 hours per week",
    "Entre 8 y 10 horas a la semana" = "Between 8 and 10 hours per week",
    "Más de 10 horas a la semana" = "More than 10 hours per week",
    # Education levels
    "No terminaré la primaria" = "I will not finish primary school",
    "Terminaré la secundaria" = "I will finish secondary school",
    "Terminaré una carrera técnica" = "I will complete a technical degree",
    "Terminaré una carrera universitaria" = "I will complete a university degree",
    "Terminaré una maestría o doctorado" = "I will complete a master's degree or doctorate",
    # Languages
    "Castellano" = "Spanish",
    "Quechua" = "Quechua",
    "Aimara" = "Aymara",
    "Una lengua amazónica (awajún, shipibo, asháninka, etc.)" = "An Amazonian language (Awajún, Shipibo, Asháninka, etc.)",
    "Una lengua extranjera (inglés, francés, alemán, etc.)" = "A foreign language (English, French, German, etc.)"
  )
  # number-year pattern like "2: 10 años" -> "2: 10 years"
  conv_num_years <- function(s) sub("^([0-9]+):\\s*([0-9]+) años$", "\\1: \\2 years", s, perl = TRUE)
  for (i in seq_along(x)) {
    sp <- split_prefix(x[i]); pre <- sp$pre; rest <- sp$rest
    if (rest %in% names(dict_exact)) {
      en <- dict_exact[[rest]]
    } else if (rest %in% names(dict_terms)) {
      en <- dict_terms[[rest]]
    } else {
      en <- conv_num_years(rest)
      en <- gsub("¿", "", en, fixed = TRUE)
      # Common sub-item phrase mappings (study strategies, class practices, attitudes)
      en <- gsub("^Resalto lo que me parece más importante$", "I highlight what seems most important to me", en)
      en <- gsub("^Intento memorizar el contenido palabra por palabra$", "I try to memorize the content word for word", en)
      en <- gsub("^Repito lo que he leído varias veces$", "I repeat what I have read several times", en)
      en <- gsub("^Reescribo mis apuntes del libro a mi cuaderno$", "I rewrite my notes from the book into my notebook", en)
      en <- gsub("^Intento comprender lo que aprendo$", "I try to understand what I learn", en)
      en <- gsub("^Se me ocurren ejemplos relacionados con lo que estoy aprendiendo$", "I come up with examples related to what I am learning", en)
      en <- gsub("^Intento relacionar lo que estoy aprendiendo con cosas que ya sé$", "I try to relate what I am learning to things I already know", en)
      en <- gsub("^Asocio lo que estudio con imágenes o situaciones que creo en mi mente$", "I associate what I study with images or situations I create in my mind", en)
      en <- gsub("^Intento explicar los conceptos con mis propias palabras$", "I try to explain the concepts in my own words", en)
      en <- gsub("^Intento deducir el significado de palabras o frases desconocidas utilizando el propio texto$", "I try to infer the meaning of unknown words or phrases using the text itself", en)
      en <- gsub("^Después de leer, me explico a mí mismo lo que he leído$", "After reading, I explain to myself what I have read", en)
      en <- gsub("^Escribo resúmenes$", "I write summaries", en)
      en <- gsub("^Elaboro mapas conceptuales o diagramas$", "I make concept maps or diagrams", en)
      en <- gsub("^Explico de qué trata el texto a otros compañeros o personas$", "I explain what the text is about to classmates or others", en)

      # Class practices - Communication / Math
      en <- gsub("^Nos pide dar nuestra opinión personal sobre lo que leemos$", "He/She asks us to give our personal opinion about what we read", en)
      en <- gsub("^Nos pide explicar por qué nos gusta o disgusta un pasaje del texto$", "He/She asks us to explain why we like or dislike a passage of the text", en)
      en <- gsub("^Nos pide que pensemos o que nos preguntemos sobre cómo se relacionan los textos que leemos con nuestra vida diaria$", "He/She asks us to think about how the texts we read relate to our daily life", en)
      en <- gsub("^Nos pide que expliquemos nuestras respuestas con detalle$", "He/She asks us to explain our answers in detail", en)
      en <- gsub("^Nos presenta textos que incluyen esquemas o mapas conceptuales$", "He/She presents texts that include outlines or concept maps", en)
      en <- gsub("^Nos pide explicar el aporte de los gráficos o dibujos del texto$", "He/She asks us to explain the contribution of the text's graphics or drawings", en)
      en <- gsub("^Nos presenta diferentes textos que tratan del mismo tema$", "He/She presents different texts on the same topic", en)
      en <- gsub("^Nos hace pensar si es que podemos confiar en lo que dice el autor en el texto$", "He/She makes us consider whether we can trust what the author says in the text", en)
      en <- gsub("^Nos pide que compartamos en grupo nuestras opiniones sobre los textos$", "He/She asks us to share our opinions about the texts in groups", en)

      en <- gsub("^Nos deja tareas que requieren pensar más y no solo aplicar fórmulas o resolver ecuaciones$", "He/She assigns tasks that require more thinking and not just applying formulas or solving equations", en)
      en <- gsub("^Nos pide explicar cómo hemos resuelto un problema o ejercicio$", "He/She asks us to explain how we solved a problem or exercise", en)
      en <- gsub("^Nos pide que pensemos o nos preguntemos sobre cómo se relacionan los temas nuevos con los anteriores$", "He/She asks us to think about how new topics relate to previous ones", en)
      en <- gsub("^Nos enseña a resolver los problemas de muchas maneras$", "He/She teaches us to solve problems in many ways", en)
      en <- gsub("^Nos pide que elaboremos nuestros propios problemas de matemática$", "He/She asks us to create our own math problems", en)
      en <- gsub("^Nos motiva a que encontremos nuestra propia forma de resolver los problemas o ejercicios de matemática$", "He/She encourages us to find our own way to solve math problems or exercises", en)
      en <- gsub("^Nos motiva a relacionar lo que aprendemos con situaciones de la vida real$", "He/She encourages us to relate what we learn to real-life situations", en)

      # Classroom climate (math)
      en <- gsub("^Durante la clase hay desorden en el salón$", "During class there is disorder in the classroom", en)
      en <- gsub("^El profesor tiene que esperar mucho tiempo para que los estudiantes se callen$", "The teacher has to wait a long time for students to be quiet", en)
      en <- gsub("^Algunos estudiantes hacen tanto desorden que se hace difícil aprender$", "Some students create so much disorder that it is hard to learn", en)
      en <- gsub("^Cuando el profesor explica algo en clase, es capaz de hacer que todos le prestemos atención$", "When the teacher explains something in class, he/she can get everyone to pay attention", en)
      en <- gsub("^El profesor mantiene el orden de las clases a pesar de todas las preguntas de los estudiantes$", "The teacher keeps order in class despite all the students' questions", en)
      en <- gsub("^Los estudiantes participan de manera ordenada en las clases$", "Students participate in an orderly way in class", en)
      en <- gsub("^Cuando un estudiante participa en clase, otros interrumpen$", "When a student participates in class, others interrupt", en)
      en <- gsub("^Los estudiantes se distraen en la clase$", "Students get distracted in class", en)

      # Math anxiety/attitudes (p15, p16)
      en <- gsub("^La matemática me da miedo$", "Math scares me", en)
      en <- gsub("^En general, me preocupa resolver ejercicios de matemática$", "In general, I worry about solving math exercises", en)
      en <- gsub("^Me pongo muy nervioso o nerviosa en un examen de matemática$", "I get very nervous in a math exam", en)
      en <- gsub("^Normalmente, la matemática me pone muy nervioso o nerviosa$", "Normally, math makes me very nervous", en)
      en <- gsub("^Me siento mal cuando pienso en resolver problemas de matemática$", "I feel bad when I think about solving math problems", en)
      en <- gsub("^Cuando hago problemas de matemática se me pone la mente en blanco y no puedo pensar claramente$", "When I do math problems my mind goes blank and I can't think clearly", en)
      en <- gsub("^Me dan miedo los exámenes de matemática$", "I am afraid of math exams", en)
      en <- gsub("^Las tareas de matemática me hacen sentir nervioso o nerviosa$", "Math homework makes me feel nervous", en)

      en <- gsub("^Me gusta la matemática$", "I like math", en)
      en <- gsub("^Me siento bien cuando tengo que hacer mis tareas de matemática$", "I feel good when I have to do my math homework", en)
      en <- gsub("^Me da pena tener que gastar mi tiempo haciendo ejercicios de matemática$", "I feel sorry to spend my time doing math exercises", en)
      en <- gsub("^Cuando tengo que hacer matemática, preferiría hacer otras cosas$", "When I have to do math, I would rather do other things", en)
      en <- gsub("^La matemática es fácil para mí$", "Math is easy for me", en)
      en <- gsub("^Me intereso por la clase de matemática$", "I am interested in math class", en)
      en <- gsub("^Me interesa que me vaya bien en matemática porque es importante para mí$", "It matters to me to do well in math because it is important to me", en)
      en <- gsub("^Si me dieran la opción de elegir, escogería aprender matemática$", "If I had the option, I would choose to learn math", en)

      # Participation/teachers
      en <- gsub("^Nos motivan a expresar nuestras ideas y opiniones en clase$", "They encourage us to express our ideas and opinions in class", en)
      en <- gsub("^Se molestan cuando nos equivocamos al participar en clase$", "They get upset when we make mistakes while participating in class", en)
      en <- gsub("^Nos hablan mucho y dejan poco espacio para que participemos$", "They talk a lot and leave little room for us to participate", en)
      en <- gsub("^Nos escuchan con interés cuando opinamos en clase$", "They listen with interest when we give our opinions in class", en)
      en <- gsub("^Nos motivan a formular preguntas durante la clase$", "They encourage us to ask questions during class", en)
      en <- gsub("^Nos plantean actividades interesantes y entretenidas que motivan nuestra participación$", "They propose interesting and engaging activities that motivate our participation", en)
      en <- gsub("^Nos piden que opinemos al final de la clase para no interrumpirlos$", "They ask us to share our opinions at the end of class so as not to interrupt", en)
      en <- gsub("^Nos demuestran que prefieren la participación de los estudiantes más destacados$", "They show that they prefer the participation of the top students", en)

      # P06 options (preparedness)
      en <- gsub("^Nada preparado$", "Not at all prepared", en)
      en <- gsub("^Poco preparado$", "A little prepared", en)
      en <- gsub("^Más o menos preparado$", "Somewhat prepared", en)
      en <- gsub("^Totalmente preparado$", "Fully prepared", en)

      # P09 reasons for reading
      en <- gsub("^Leo porque lo disfruto mucho$", "I read because I enjoy it a lot", en)
      en <- gsub("^Leo porque es divertido$", "I read because it is fun", en)
      en <- gsub("^Leo porque me gusta mucho$", "I read because I like it a lot", en)
      en <- gsub("^Leo porque creo que es interesante$", "I read because I think it is interesting", en)
      en <- gsub("^Leo porque creo que la lectura es útil$", "I read because I think reading is useful", en)
      en <- gsub("^Leo porque es importante para mí$", "I read because it is important to me", en)
      en <- gsub("^Leo porque no quiero decepcionar a los demás$", "I read because I do not want to disappoint others", en)
      en <- gsub("^Leo porque me sentiré culpable si no lo hago$", "I read because I would feel guilty if I did not", en)
      en <- gsub("^Leo porque eso es lo que los demás esperan que haga$", "I read because that is what others expect me to do", en)
      en <- gsub("^Leo porque tengo que demostrarme a mí mismo que puedo sacar buenas notas en Comunicación$", "I read because I have to prove to myself that I can get good grades in Communication", en)
      en <- gsub("^Leo porque me castigarán si no leo$", "I read because I will be punished if I do not", en)
      en <- gsub("^Leo porque lo veo necesario para no sentirme mal conmigo mismo$", "I read because I see it as necessary so I do not feel bad about myself", en)

      # P11 communication class statements
      en <- gsub("^Me gusta lo que leo en la clase$", "I like what I read in class", en)
      en <- gsub("^Los textos que nos dan en clase son muy difíciles de comprender$", "The texts we are given in class are very difficult to understand", en)
      en <- gsub("^En la clase, me dan textos interesantes para leer$", "In class, we are given interesting texts to read", en)
      en <- gsub("^Disfruto aprender cosas nuevas en la clase$", "I enjoy learning new things in class", en)
      en <- gsub("^Los textos que nos dan en clase son muy largos$", "The texts we are given in class are very long", en)
      en <- gsub("^En general, me interesa lo que leo en la clase$", "In general, I am interested in what I read in class", en)
      en <- gsub("^Los textos que nos dan en clase tienen palabras muy difíciles$", "The texts we are given in class have very difficult words", en)
      en <- gsub("^En las clases, me dan textos con información nueva$", "In class, we are given texts with new information", en)
      en <- gsub("^Los textos que leemos son para estudiantes de nuestra edad$", "The texts we read are for students our age", en)
      en <- gsub("^Los textos que nos dan en clase son muy aburridos$", "The texts we are given in class are very boring", en)

      # P17 social personal
      en <- gsub("^Nos pide recordar de memoria las fechas y los lugares en los que sucedieron los hechos$", "He/She asks us to memorize the dates and places where events happened", en)
      en <- gsub("^Nos pide identificar las diversas causas y consecuencias de un hecho$", "He/She asks us to identify the various causes and consequences of an event", en)
      en <- gsub("^Nos pide aprender de memoria los hechos tal como los explicó$", "He/She asks us to memorize the facts as explained", en)
      en <- gsub("^Nos presenta diversas maneras de explicar un mismo hecho$", "He/She presents different ways of explaining the same event", en)
      en <- gsub("^Nos dice que solo existe una verdadera versión de los hechos$", "He/She says there is only one true version of events", en)
      en <- gsub("^Nos pide utilizar información confiable al defender un punto de vista$", "He/She asks us to use reliable information when defending a point of view", en)
      en <- gsub("^Nos dice que los personajes importantes son los que solucionan los problemas de la sociedad$", "He/She says important figures are the ones who solve society's problems", en)

      # P18 statements
      en <- gsub("^Considero que todas las personas de la comunidad influyen sobre los hechos que ocurren en la sociedad$", "I believe everyone in the community influences the events that occur in society", en)
      en <- gsub("^Considero que las decisiones de los personajes importantes son las que determinan lo que ocurre en la sociedad$", "I believe the decisions of important figures determine what happens in society", en)
      en <- gsub("^Para comprender un hecho es necesario conocer las características de la época y del lugar en el que ocurre$", "To understand an event it is necessary to know the characteristics of the time and place in which it occurs", en)
      en <- gsub("^Algunas ideas y costumbres del pasado se mantienen hasta la actualidad$", "Some ideas and customs from the past remain to this day", en)
      en <- gsub("^Las ideas y costumbres de la actualidad son totalmente distintas a las del pasado$", "Today's ideas and customs are completely different from those of the past", en)
      en <- gsub("^Considero que un hecho puede tener varias causas$", "I believe an event can have several causes", en)
      en <- gsub("^Considero que un hecho puede tener varias consecuencias$", "I believe an event can have several consequences", en)

      # P19 statements
      en <- gsub("^Considero que para comprender un hecho es necesario utilizar información de diversas fuentes$", "I believe that to understand an event it is necessary to use information from various sources", en)
      en <- gsub("^Considero que cualquier información es confiable, sin importar de donde proviene$", "I believe that any information is reliable regardless of where it comes from", en)
      en <- gsub("^Las personas explican los hechos desde su punto de vista$", "People explain events from their point of view", en)
      en <- gsub("^Un mismo hecho de la sociedad puede ser comprendido de distinta manera$", "The same event in society can be understood in different ways", en)
      en <- gsub("^Existe una sola versión verdadera de los hechos que ocurren en la sociedad$", "There is only one true version of the events that occur in society", en)
      en <- gsub("^Para defender un punto de vista sobre algún hecho es necesario usar información confiable$", "To defend a point of view about an event it is necessary to use reliable information", en)
      en <- gsub("^Los problemas sociales se pueden resolver de una sola manera$", "Social problems can be solved in only one way", en)
      en <- gsub("^Para solucionar los problemas de la sociedad se debe hacer lo mismo que los países desarrollados$", "To solve society's problems we must do the same as developed countries", en)
      en <- gsub("^Todas las personas pueden contribuir a la solución de los problemas$", "Everyone can contribute to solving problems", en)
      en <- gsub("^La solución a los problemas de la sociedad depende de los personajes importantes$", "The solution to society's problems depends on important figures", en)

      # P20 school belonging/safety
      en <- gsub("^Prefiero faltar a la escuela$", "I prefer to skip school", en)
      en <- gsub("^Mi escuela es un lugar donde me siento solo$", "My school is a place where I feel lonely", en)
      en <- gsub("^Preferiría estudiar en otra escuela$", "I would prefer to study at another school", en)
      en <- gsub("^Siento que no pertenezco a esta escuela$", "I feel that I do not belong at this school", en)
      en <- gsub("^Me siento orgulloso de estar en esta escuela$", "I feel proud to be at this school", en)
      en <- gsub("^Mi escuela es un lugar donde me siento como un extraño$", "My school is a place where I feel like a stranger", en)
      en <- gsub("^Me siento seguro en el camino cuando voy de mi casa a la escuela$", "I feel safe on the way when I go from my home to school", en)
      en <- gsub("^Me siento seguro en los salones de clase de mi escuela$", "I feel safe in my school's classrooms", en)
      en <- gsub(
        "Me siento seguro en otros lugares de la escuela (por ejemplo, patios, pasillos, baños, etc.)",
        "I feel safe in other places at school (e.g., yards, hallways, bathrooms, etc.)",
        en,
        fixed = TRUE
      )

      # crude accent strip for readability
      en <- chartr("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN", en)
    }
    out[i] <- paste0(pre, en)
  }
  out
}

# Load manual overrides from TSV (columns: es, en). Returns named vector en[es]
load_translation_overrides <- function(tsv_path) {
  if (!file.exists(tsv_path)) return(character())
  df <- tryCatch(read.delim(tsv_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8"), error = function(e) NULL)
  if (is.null(df) || !all(c("es","en") %in% names(df))) return(character())
  es <- as.character(df$es); en <- as.character(df$en)
  keep <- nzchar(es) & !is.na(es) & !is.na(en) & nzchar(en)
  stats::setNames(en[keep], es[keep])
}

# Apply overrides to a character vector, preserving code prefixes
apply_overrides_vec <- function(x, overrides) {
  if (length(overrides) == 0 || length(x) == 0) return(x)
  x <- as.character(x); x[is.na(x)] <- ""
  res <- character(length(x))
  split_prefix <- function(s) {
    m <- regexpr("^(?:p[0-9]{2}(?:_[0-9]{2})?: |[0-9]+:\\s*)", s, perl = TRUE)
    if (m[1] > 0) {
      pre <- substr(s, m[1], m[1] + attr(m, 'match.length') - 1)
      rest <- substr(s, m[1] + attr(m, 'match.length'), nchar(s))
      list(pre = pre, rest = trimws(rest))
    } else list(pre = "", rest = trimws(s))
  }
  for (i in seq_along(x)) {
    sp <- split_prefix(x[i]); pre <- sp$pre; rest <- sp$rest
    val <- overrides[rest]
    en <- if (length(val) == 1 && !is.na(val)) as.character(val) else rest
    res[i] <- paste0(pre, en)
  }
  res
}

write_bilingual_longtable <- function(df_es, df_en, out_tex, title = "ENLA 2024 — Student Questionnaire (Bilingual ES–EN)") {
  # Ensure alignment by item
  ord <- match(df_es$item, df_en$item)
  df_en <- df_en[ord, ]

  header <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{microtype}",
    "\\usepackage{enumitem}",
    "\\usepackage{array}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{ragged2e}",
    "\\setlength{\\parindent}{0pt}",
    "\\sloppy",
    "\\newcolumntype{L}{>{\\RaggedRight\\arraybackslash}p{0.48\\textwidth}}",
    # No title/date/TOC per user request
    "\\begin{document}",
    "\\newpage",
    "\\begin{longtable}{@{}LL@{}}",
    "\\toprule",
    "\\textbf{Español} & \\textbf{English} \\\\ ",
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    "\\textbf{Español} & \\textbf{English} \\\\ ",
    "\\midrule",
    "\\endhead",
    "\\bottomrule \\\\ ",
    "\\endfoot",
    "\\bottomrule"
  )

  body <- character()
  for (i in seq_len(nrow(df_es))) {
    item <- toupper(df_es$item[i])
    q_es <- ifelse(is.na(df_es$question[i]) || !nzchar(df_es$question[i]), "(sin texto)", df_es$question[i])
    q_en <- ifelse(is.na(df_en$question_en[i]) || !nzchar(df_en$question_en[i]), "(no question text)", df_en$question_en[i])
    subs_es <- df_es$subitems_raw[[i]]; if (is.null(subs_es)) subs_es <- character()
    subs_en <- df_en$subitems_en[[i]]; if (is.null(subs_en)) subs_en <- character()
    opts_es <- df_es$options_list[[i]]; if (is.null(opts_es)) opts_es <- character()
    opts_en <- df_en$options_en[[i]]; if (is.null(opts_en)) opts_en <- character()

    # Section row (spanning both columns)
    body <- c(body, paste0("\\multicolumn{2}{@{}l@{}}{\\large\\textbf{", tex_esc(item), "}} \\\\ "))

    # Question row
    body <- c(body, paste0(tex_esc(q_es), " & ", tex_esc(q_en), " \\\\"))

    # Sub-items row (only if any)
    if (length(subs_es) > 0 || length(subs_en) > 0) {
      left <- if (length(subs_es) > 0) paste0("\\textbf{Sub-items}\\par\\begin{itemize}[leftmargin=*]", paste0("\\item ", tex_esc(subs_es), collapse = "\n"), "\\end{itemize}") else "\\textbf{Sub-items}\\par --"
      right <- if (length(subs_en) > 0) paste0("\\textbf{Sub-items}\\par\\begin{itemize}[leftmargin=*]", paste0("\\item ", tex_esc(subs_en), collapse = "\n"), "\\end{itemize}") else "\\textbf{Sub-items}\\par --"
      body <- c(body, paste0(left, " & ", right, " \\\\"))
    }

    # Options row (only if any)
    if (length(opts_es) > 0 || length(opts_en) > 0) {
      left <- if (length(opts_es) > 0) paste0("\\textbf{Opciones}\\par\\begin{itemize}[leftmargin=*]", paste0("\\item ", tex_esc(opts_es), collapse = "\n"), "\\end{itemize}") else "\\textbf{Opciones}\\par --"
      right <- if (length(opts_en) > 0) paste0("\\textbf{Options}\\par\\begin{itemize}[leftmargin=*]", paste0("\\item ", tex_esc(opts_en), collapse = "\n"), "\\end{itemize}") else "\\textbf{Options}\\par --"
      body <- c(body, paste0(left, " & ", right, " \\\\"))
    }

    # Spacer
    body <- c(body, "\\addlinespace[4pt]")
  }

  footer <- c(
    "\\end{longtable}",
    "\\end{document}"
  )

  lines <- c(header, body, footer)
  writeLines(lines, con = out_tex)
}

export_student_questions_bilingual_clean <- function() {
  # Build ES and EN structures using existing helpers
  df_es <- build_student_questions()
  # Prepare initial EN via local translator
  init_q_en <- translate_local_vec(df_es$question)
  init_sub_en <- lapply(df_es$subitems_raw, translate_local_vec)
  init_opt_en <- lapply(df_es$options_list, translate_local_vec)

  # Create TSV with all unique ES and initial EN if missing
  tsv_path <- file.path(DATA_DIR, "translations_manual.tsv")
  if (!file.exists(tsv_path)) {
    es_vec <- unique(c(
      as.character(df_es$question),
      unlist(df_es$subitems_raw, use.names = FALSE),
      unlist(df_es$options_list, use.names = FALSE)
    ))
    es_vec <- es_vec[!is.na(es_vec) & nzchar(es_vec)]
    en_vec <- translate_local_vec(es_vec)
    writeLines(
      c("es\ten", paste0(es_vec, "\t", en_vec)),
      con = tsv_path,
      useBytes = TRUE
    )
    message("Created translations TSV: ", tsv_path)
  }

  # Load overrides and apply
  overrides <- load_translation_overrides(tsv_path)
  q_en <- apply_overrides_vec(init_q_en, overrides)
  sub_en <- lapply(df_es$subitems_raw, function(v) apply_overrides_vec(translate_local_vec(v), overrides))
  opt_en <- lapply(df_es$options_list, function(v) apply_overrides_vec(translate_local_vec(v), overrides))

  df_en <- tibble::tibble(
    item = df_es$item,
    question_en = q_en,
    subitems_en = sub_en,
    options_en = opt_en
  )

  out_tex <- file.path(REPORTS_DIR, "student_questionnaire_bilingual_clean.tex")
  write_bilingual_longtable(df_es, df_en, out_tex)
  message("Bilingual (clean) LaTeX report written: ", out_tex)
  invisible(out_tex)
}

# Auto-run if executed directly
if (sys.nframe() == 0) {
  export_student_questions_bilingual_clean()
}
