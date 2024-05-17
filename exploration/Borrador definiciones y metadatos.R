##########
tabPanel(
  lowercaseTitle("Definiciones y Metadatos"),
  br(),
  ### FRANKIE: para cada título de sección escríbelo adentro de lo siguiente: tags$ul(sectionTitle("título", "20px")),
  tags$ul(sectionTitle("Título:", "20px")),
  tags$ul(
    ########### FRANKIE cuando algo se escribe adentro de un tags$li(HTML()) eso crea un item aparte
    ### FRANKIE: Cuando algo se pone entre <b> y </b> se renderiza en negritas
    tags$li(HTML("<b>Incumplimiento de órdenes de protección (Artículo 2.8):</b> Cualquier violación a sabiendas de una orden de protección expedida.")),
    tags$li(HTML("<b>Maltrato (Artículo 3.1):</b> Toda persona que empleare fuerza física ...")),
    tags$li(HTML("<b>Maltrato Agravado (Artículo 3.2):</b><br>

######## FRANKIE: aqui hay un <ul> y </ul>. Esto crea una lista de items donde los <li> vienen siendo cada bullet point individualmente
<ul>
  <li>a. bullet 1; o</li>
  <li>b. bullet 2; o</li>
  <li>c. bullet 3; o</li>
</ul>
"))
    
    ,
    
    
    tags$li(HTML(
      "<b>Agresión Sexual Conyugal (Artículo 3.5): </b> DEFINICION :
<ul>          
  <li>a. bullet 1; 
<li>b. bullet 2; 
<li>c. bullet 3;")),
    
  ),
  br(),
  ### FRANKIE: para cada título de sección escríbelo adentro de lo siguiente: tags$ul(sectionTitle("título", "20px")),
  tags$ul(sectionTitle("Fiscalías de Puerto Rico:")),
  
)
########Sistema de Notificación de Muertes Violentas######
tags$ul(sectionTitle("Sistema de Notificación de Muertes Violentas:", "20px")),
tags$li(HTML("<b>Violencia:</b> El uso intencional de la fuerza o el poder físico, de hecho, o como amenaza, contra uno mismo, otra persona o un grupo o comunidad, que cause o tenga gran probabilidad de causar lesiones, muerte, daños psicológicos, trastornos del desarrollo o privaciones.
")),
tags$li(HTML("<b>Muertes violentas :</b> PRVDRS define una muerte violenta como: Suicidio (incluyendo terrorismo), Homicidio (incluyendo terrorismo), Intervención legal (excluyendo terrorismo, ejecuciones por pena de muerte, o actos de guerra), Eventos con intención no Determinada, Heridas por arma de fuego no intencional.")), 
tags$li(HTML("<b>Homicidio:</b> Es un delito de resultado por el cual la conducta intencionada de un sujeto provoca la muerte de otro.
 ")), 
tags$li(HTML("<b>Suicidio Único:</b> Se refiere a un acto en el cual una persona termina deliberadamente con su propia vida, sin la participación o implicación de otras personas en el proceso. Este término implica que el individuo toma la decisión y ejecuta el acto suicida de manera independiente, sin ningún intento de causar la muerte de otras personas o de involucrar a terceros en el evento. 
 ")), 
tags$li(HTML("<b>Homicidio múltiple:</b>Se refiere a un acto en el cual una persona causa la muerte de múltiples individuos en un solo incidente.")), 
tags$li(HTML("<b>Homicidio seguido de suicidio:</b> Se refiere a un acto en el cual una persona causa la muerte de otra(s) y luego se quita la vida. Este tipo de incidente implica dos acciones distintas pero relacionadas: primero, el homicida comete el acto de matar a otra persona, y luego, la misma persona toma su propia vida.
 ")), 
tags$li(HTML("<b>Homicidio único:</b> Se refiere a un acto en el cual una persona causa la muerte de otra en un evento específico. ")), 
tags$li(HTML("<b>Homicidio(s) seguido de suicidio(s), (más de dos víctimas):</b> Se refiere a un acto extremadamente trágico en el cual una o más personas causan la muerte de múltiples individuos antes de acabar con sus propias vidas en un solo evento
 ")), 
tags$li(HTML("<b>Muerte accidental por arma de fuego:</b> Evento en el cual una persona pierde la vida como resultado involuntario de la manipulación, manejo o uso incorrecto de un arma de fuego. Este tipo de incidente ocurre cuando un individuo dispara un arma de manera accidental, ya sea debido a un descuido, falta de conocimiento sobre el funcionamiento seguro del arma, o por la manipulación indebida de la misma. ")), 
tags$li(HTML("<b>Muerte no determinada:</b> Caso en el cual las circunstancias que rodean el fallecimiento de una persona no pueden ser claramente establecidas o comprendidas mediante la evidencia disponible en el momento de la investigación. Este término se utiliza cuando no hay suficiente información o evidencia forense para determinar con certeza si la muerte fue el resultado de causas naturales, accidentales, suicidas u homicidas.
 ")), 


####### Departamento de la Familia ######
tags$ul(sectionTitle("Departamento de la Familia:", "20px")),

tags$li(HTML("<b>Menores:</b> Individuos que se encuentran bajo la edad legal de mayoría de edad en un contexto específico, lo que generalmente implica que aún no han alcanzado la edad en la que se les considera plenamente responsables de sus acciones según la ley.
")),
tags$li(HTML("<b>Maltrato a Menores:</b> Toda acción u omisión intencional del padre, de la madre o persona responsable del o de la menor que ocasione o ponga en riesgo de sufrir un daño o perjuicio a su salud e integridad física, mental o emocional.
 ")), 
tags$li(HTML("<b>Abuso Sexual:</b> Incurrir en conducta sexual en presencia de un o una menor o que se utilice, voluntaria o involuntariamente, para ejecutar conducta sexual dirigida a satisfacer los deseos sexuales. También se considera cualquier acto que, de procesarse por la vía criminal, configuraría cualesquiera de varios delitos de índole sexual, tales como agresión sexual, actos lascivos, comercio de personas para actos sexuales, exposiciones obscenas, proposición obscena, producción de pornografía infantil, entre otros delitos reconocidos en el Código Penal de Puerto Rico.
 ")), 
tags$li(HTML("<b>Explotación:</b> Conducta obscena o utilización de una persona menor de edad para ejecutar conducta obscena. explotación de una persona menor de edad o que se permita que otra persona lo explote, incluyendo –pero sin limitarse– a utilizarla para ejecutar conducta obscena, con el fin de lucrarse o de recibir algún otro beneficio.
 ")), 
tags$li(HTML("<b>Maltrato Físico:</b> Se refiere a cualquier trauma, lesión o condición no accidental, causada en un episodio o varios, incluyendo la falta de alimentos que, de no ser atendida, pone en riesgo la vida y salud de la persona menor de edad.
 ")), 
tags$li(HTML("<b>Negligencia:</b> Es un tipo de maltrato que consiste en no cumplir con la obligación de proveer a las personas menores de edad de manera adecuada los alimentos, ropa, albergue, educación, atención a su salud, poca supervisión, no visitar, ni mantener contacto con el o la menor o incurrir en alguna de las razones reconocidas en el Código Civil de Puerto Rico para que una persona sea privada de patria potestad entre otros.
 ")), 
tags$li(HTML(
  "<b>Negligencia Educativa: </b> La negligencia institucional es cuando a una persona menor de edad, que está en un hogar de crianza, centro de cuidado sustituto o en una institución pública o privada, de cuido, educación, tratamiento o detención, se le cause daño o se ponga en riesgo de sufrir daño a su salud e integridad física, mental o emocional, incluyendo –pero sin limitarse– a abuso sexual. La negligencia institucional, ya sea conocida o que se sospeche que ocurre, o que ocurre como resultado de la política, prácticas y condiciones imperantes en la institución, la puede cometer:
<ul>          
  <li>a. operador u operadora de un hogar de crianza; 
<li>b. cualquier empleado, empleada, funcionario o funcionaria que ofrezca servicios de cuido o que tenga bajo su control o custodia a una persona menor de edad para su cuido, educación, tratamiento o detención
; 
),
tags$li(HTML("<b>Negligencia Emocional:</b> Se define como causar menoscabo o afectar la capacidad intelectual o emocional de la persona menor de edad dentro de lo que se considera normal para su edad y entorno cultural.
 ")), 
tags$li(HTML("<b>Negligencia Médica:</b> Situaciones en las que los proveedores de atención médica, como médicos, enfermeras u otros profesionales de la salud, no brindan el nivel adecuado de atención y cuidado a pacientes menores de edad, lo que resulta en daños físicos, emocionales o psicológicos para el paciente. Esto puede incluir errores en el diagnóstico, tratamiento inapropiado, falta de seguimiento adecuado, o cualquier otro acto u omisión que pueda considerarse una violación del estándar de cuidado aceptado en la práctica médica.
 ")), 
tags$li(HTML("<b>Trata Humana:</b> Se define como la captación, traslado, transporte, acogida o recepción de una persona utilizando la violencia, amenaza, engaño, rapto, fuerza, abuso de poder, abuso de una situación de vulnerabilidad u otros elementos de coacción, con el fin de someterla a explotación y lucrarse con su actividad. ")), 


####Departamento del Trabajo y Recursos Humanos#####
tags$ul(sectionTitle("Departamento del Trabajo y Recursos Humanos:", "20px")),

tags$li(HTML("<b>Tasa:</b> Es la proporción que representa el número de sucesos del tipo que sea en la población en que se producen.
 ")), 
tags$li(HTML("<b>Participación laboral:</b> Grado en que la población en edad de trabajar participa en la fuerza laboral de un país o región durante un período de tiempo determinado.
 ")), 
 
###### Administración de Vivienda Pública ######
tags$ul(sectionTitle("Administración de Vivienda Pública:", "20px")),

tags$li(HTML("<b>Vivienda Pública:</b> Vivienda que es proporcionada, administrada o subsidiada por el gobierno o entidades gubernamentales con el objetivo de brindar alojamiento a personas o familias que tienen dificultades para acceder a una vivienda adecuada en el mercado privado debido a limitaciones económicas o sociales. Estas viviendas suelen estar dirigidas a personas de bajos ingresos, familias en situación de pobreza, personas sin hogar, o aquellos que enfrentan otras formas de vulnerabilidad social.
 ")), 
tags$li(HTML("<b>Violencia Doméstica:</b> Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, excónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo. ")), 
tags$li(HTML("<b>Región:</b> Se refiere a una división geográfica o área delimitada que comparte características similares, ya sea geográficas, culturales, económicas, políticas o administrativas. 
 Subdivisión territorial establecida por las autoridades gubernamentales para propósitos de administración y gestión local. Estas divisiones pueden variar en tamaño y alcance dependiendo del país y su estructura administrativa.
 ")), 
 
 ###### Negociado de Policía #####
tags$ul(sectionTitle("Negociado de Policía:", "20px")),
tags$li(HTML("<b>Adultas Desaparecidas:</b> Mujeres adultas cuya ubicación y paradero son desconocidos y no pueden ser determinados por sus familiares, amigos, o autoridades competentes. Esta situación puede surgir por una variedad de razones, que van desde accidentes, secuestros, desastres naturales, hasta decisiones voluntarias de la persona de abandonar su entorno sin dejar rastro.
 ")), 
tags$li(HTML("<b>Adultas Localizadas:</b> Mujeres adultas cuyo paradero ha sido identificado y confirmado después de que se les haya reportado como desaparecidas. ")), 
tags$li(HTML("<b>Adultas Sin Localizar:</b> Mujeres adultas cuyo paradero no ha sido identificado ni confirmado después de haber sido reportadas como desaparecidas. Estas personas pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio.
 ")), 
tags$li(HTML("<b>Menores Desaparecidas:</b> Menores femeninas cuya ubicación y paradero son desconocidos y no pueden ser determinados por sus familiares, amigos, o autoridades competentes. Esta situación puede surgir por una variedad de razones, que van desde accidentes, secuestros, desastres naturales, hasta decisiones voluntarias de la persona de abandonar su entorno sin dejar rastro.  ")), 
tags$li(HTML("<b>Menores Localizadas:</b> Menores femeninas cuyo paradero ha sido identificado y confirmado después de que se les haya reportado como desaparecidos. ")), 
tags$li(HTML("<b>Menores sin Localizar:</b> Menores femeninas cuyo paradero no ha sido identificado ni confirmado después de haber sido reportados como desaparecidos. Estas personas pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio.
 ")), 
tags$li(HTML("<b>Incidencia:</b> Se refiere al número de incidentes o delitos que han sido reportados o registrados por las fuerzas policiales durante un período de tiempo específico en una determinada área geográfica. Esta métrica es utilizada por las autoridades policiales y otros organismos encargados de hacer cumplir la ley para medir y analizar la cantidad y el tipo de delitos que ocurren en una comunidad o jurisdicción particular. ")), 
tags$li(HTML("<b>Violencia doméstica:</b> Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, ex cónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo. ")), 
tags$li(HTML("<b>Víctima:</b> Persona que ha sufrido daño físico, emocional, psicológico o financiero como resultado de un acto delictivo, un accidente, un desastre natural, o cualquier otro evento traumático.
 ")), 
tags$li(HTML("<b>Área Policiaca:</b> Región o zona geográfica específica que está asignada a un cuerpo de policía o agencia de aplicación de la ley para llevar a cabo funciones de vigilancia, patrullaje y protección del orden público. Estas áreas son delimitadas y definidas por las autoridades policiales con el objetivo de organizar y distribuir eficazmente los recursos policiales para atender las necesidades de seguridad y aplicación de la ley en una comunidad o jurisdicción determinada.
 ")), 
 
 
 ###### Oficina de la Procuradora de la Mujer #######
tags$ul(sectionTitle("Oficina de la Procuradora de la Mujer:", "20px")),
tags$li(HTML(
  "<b>Feminicidios: </b> Es el crimen que consiste en matar intencionalmente a mujeres por el hecho de ser mujeres o de identificarse como tales. Las definiciones más amplias incluyen cualquier asesinato de mujeres o niñas, o el asesinato involuntario o indirecto de mujeres o niñas, «tal como demuestran algunos casos de violencia doméstica que podrían provocar la muerte de mujeres». El concepto «adquirió importancia en el marco del movimiento feminista de la década de 1970 cuando surge la expresión ‘femicidio’ como alternativa al término neutro ‘homicidio’, con el fin de reconocer y visibilizar la opresión, la desigualdad y la violencia sistemática» contra las mujeres que, en su forma más extrema, culmina en la muerte. El femicidio puede presentarse bajo diferentes formas e incluye los siguientes actos:
   <ul>          
   <li>a. Femicidio íntimo, perpetrado por una pareja actual o anterior, generalmente durante o después de una relación ya violenta (por ejemplo, de violencia doméstica o violencia sexual);
 <li>b. el llamado crimen de honor (o asesinato de o por honor); 
 <li>c. el femicidio relacionado con la dote, que ocurre en un contexto de conflicto entre las familias de dos cónyuges recién casados, y es generalmente cometido por la familia política que cuestiona sobre todo el importe de la dote;
<li>d. el femicidio no íntimo, cometido por un agresor que no tiene una relación íntima con la víctima, que está muy difundido en algunas partes de América Latina y que, por lo general, está precedido de actos de violencia sexual o tortura.
;")),
),
tags$li(HTML("<b>Tendencia:</b> Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo.
 ")), 
tags$li(HTML("<b>Acecho (A):</b> Es una persona, en la mayoría de las ocasiones mujer que sufre o es sometida a un patrón o la repetición de una conducta mediante la cual se mantiene de manera constante o repetida una vigilancia, proximidad física o visual sobre una persona específica. ")), 
tags$li(HTML("<b>Agresión sexual (AS):</b> Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.
  ")), 
tags$li(HTML("<b>Discrimen de género (DG):</b> Hace referencia a «toda distinción, exclusión o restricción» que se realice en función del género de una persona con el objetivo o resultado de menoscabar o anular el reconocimiento, goce o ejercicio de sus derechos humanos. A menudo es consecuencia de los mitos y estereotipos de género tales como: las mujeres son las más aptas para ocuparse de la educación de los hijos, cocinar o limpiar, o para realizar trabajos de secretaría, enseñanza o enfermería, mientras que los hombres son líderes, buenos en economía y comercio. Esto ha dado lugar a un acceso desigual al mercado laboral, así como a un salario desigual para puestos similares, al sostenerse que las mujeres tienen peores resultados que los hombres en determinados ámbitos y, con ello, a una discriminación por género. ")), 
tags$li(HTML("<b>Violencia doméstica (VD):</b> Definición que ofrece la Ley Núm. 54 de 1989 que sigue vigente en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia psicológica, intimidación o persecución contra una persona por parte de su cónyuge, ex cónyuge, una persona con quien cohabita o haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona o para causarle grave daño emocional. La Ley Núm. 54 incluyó además como delito la agresión sexual entre personas que cohabitan o matrimonios como violencia doméstica. ")), 
tags$li(HTML("<b>Violencia en cita (VC):</b> Violencia cometida por una persona que está o ha estado en una relación social de carácter romántico o íntimo con la víctima. La existencia de dicha relación se determinará con base en la declaración de la parte informante y teniendo en cuenta la duración de la relación, el tipo de relación y la frecuencia de interacción entre las personas involucradas en la relación. A los efectos de esta definición: La violencia en el noviazgo incluye, pero no se limita a, abuso sexual o físico o la amenaza de tal abuso. La violencia en el noviazgo no incluye actos cubiertos por la definición de violencia doméstica.
 ")), 
tags$li(HTML("<b>CRIAS:</b> Centro de Respuesta Integrada de Apoyo y Servicios para la Mujer. La misma surgió de la necesidad imperante de trabajar con el problema de la desigualdad que existe contra las mujeres y trabajar particularmente con la violencia doméstica desde una perspectiva dirigida hacia la validación, orientación y coordinación de servicios de apoyo.
 El Centro CRIAS establece las bases para un modelo de prevención, intervención y fiscalización de los diferentes tipos de violencia que nos permite levantar información de las víctimas sobrevivientes participantes, obtener análisis de experiencias personales y manejo de actitudes ante el problema. En el mismo, se ofrecen servicios de orientación, coordinación de servicios y referidos a mujeres víctimas/sobrevivientes de violencia doméstica, agresión sexual, acecho y otras manifestaciones de violencia por razón de género. 
 ")),
tags$li(HTML("<b>Identidad de género:</b> Se refiere a la manera en que una persona se identifica, cómo se reconoce o se expresa sobre sí misma, en cuanto al género que puede corresponder o no a su sexo biológico o asignado en su nacimiento.
 ")),
tags$li(HTML("<b>:</b> Término utilizado para describir características, atributos o cualidades asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino.
 ")),
tags$li(HTML("<b>Masculino:</b> ")),
tags$li(HTML("<b>Femenino:</b> Se refiere a características, atributos o cualidades asociadas tradicionalmente con las mujeres o lo que se considera típicamente propio del género femenino. ")),
tags$li(HTML("<b>Trans:</b> Abreviatura comúnmente utilizada para referirse a personas que son transgénero o que tienen una identidad de género diferente de aquella que se les asignó al nacer. Las personas transgénero pueden identificarse como hombre, mujer, ambos, ninguno o con un género diferente al binario tradicional de hombre y mujer.
 ")),
tags$li(HTML("<b>Orientaciones:</b> Direcciones o inclinaciones hacia las que se dirige o enfoca algo.
 ")),
 
 
######## Departamento de Corrección y Rehabilitación #######
tags$ul(sectionTitle("Departamento de Corrección y Rehabilitación:", "20px")),

tags$li(HTML("<b>Ley 54:</b> Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional.
 ")),
tags$li(HTML("<b>Programas de Comunidad:</b> Son programas de tratamientos establecidos para que las personas convictas cumplan parte de su sentencia fuera de la institución penal. Su finalidad es promover que los convictos que estén capacitados para reintegrarse a la sociedad puedan hacerlo como parte de su rehabilitación moral y social ")),
tags$li(HTML("<b>Investigaciones realizadas:</b> Proceso sistemático y metódico de recopilación, análisis y evaluación de información con el objetivo de obtener conclusiones, resolver problemas o generar conocimiento en un campo específico.  ")),
tags$li(HTML("<b>Personas sentenciadas:</b> Pronunciamiento que hace el juez o la jueza sobre la pena que se le impone a una persona acusada luego de que se determina que es culpable de cometer un delito. ")),
tags$li(HTML("<b>Programa de Supervisión Electrónica:</b> El Programa de Monitoreo Electrónico cuenta con la Unidad Especializada de Monitoreo Electrónico (Unidad) compuesta por Oficiales Correccionales, la cual tiene la responsabilidad de supervisar y monitorear a los participantes pertenecientes al programa. Esta supervisión conlleva el verificar y atender las alertas que se activan a través del sistema de transmisión electrónica, activar el protocolo, solicitar apoyo inter agencial, avisar a la víctima y administrar pruebas toxicológicas, entre otras. ")),


######### Administración de Tribunales #######
tags$ul(sectionTitle("Administración de Tribunales:", "20px")),
tags$li(HTML("<b>Organización:</b> Poder Judicial de Puerto Rico, Oficina de Administración de los Tribunales, Directoria de Operaciones, Oficina de Estadísticas. ")),
tags$li(HTML("<b>Orden de Protección:</b> Es un remedio civil expedido por escrito bajo el sello de un Tribunal, en la cual se dictan las medidas a una persona agresora para que ésta se abstenga de incurrir o llevar a cabo determinados actos o conducta constitutivos de violencia doméstica. ")),
tags$li(HTML("<b>Solicitudes de órdenes de protección:</b> Se define como todas las peticiones de orden de protección realizadas en el periodo de tiempo de interés en la región judicial especificada.
 ")),
tags$li(HTML("<b>Ley 148:</b> Conocida como la “Ley para la Protección de las Víctimas de Violencia Sexual en Puerto Rico”, según enmendada, establece los mecanismos para la expedición de órdenes de protección para víctimas de los delitos de agresión sexual, actos lascivos, acoso sexual e incesto. ")),
tags$li(HTML("<b>Ley 54:</b> La Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional")),
tags$li(HTML("<b>Violencia Sexual:</b> Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.
 ")),
tags$li(HTML("<b>Región Judicial:</b> Se refiere a la región judicial a la que corresponden los datos informados. El Tribunal de Primera Instancia se distribuye territorialmente en trece regiones judiciales. Cada región judicial está compuesta por un centro judicial y sus respectivas salas superiores y municipales. ")),
tags$li(HTML("<b>Peticionaria:</b> Persona que solicita una orden de protección. ")),
tags$li(HTML("<b>Sexo:</b> Indica si la persona que solicita la orden de protección, en el periodo de tiempo de interés en la región judicial especificada, se identifica como hombre o mujer. ")),
tags$li(HTML("<b>Año Fiscal:</b> Período de 12 meses comprendido entre el 1ro de julio de un año y el 30 de junio del año siguiente, y que se usa como el calendario presupuestario de las agencias públicas. ")),
tags$li(HTML("<b>Órdenes de protección ex parte:</b> Es una orden emitida por el Tribunal de Primera Instancia luego de escuchar a la parte peticionaria (persona que solicita la orden) y hacer una determinación provisional sobre los hechos. ")),

##################################################
tags$li(HTML(
  "<b>Acoso Sexual: </b> El Código Penal de Puerto Rico establece que el delito de acoso sexual se configura en una relación laboral, docente o de prestación de servicios, e implica lo siguiente:
   <ul>          
   <li>a. solicitar favores de naturaleza sexual para sí o para un tercero y sujetar las condiciones de trabajo, docencia o servicios a su cumplimiento.; 
 <li>b. mediante comportamiento sexual, provocar una situación con conocimiento de que resultará intimidatoria, hostil o humillante para la víctima.
;")),
),
tags$li(HTML("<b>Actos Lascivos:</b> El Código Penal de Puerto Rico define el delito de actos lascivos como aquel en el cual, sin intentar consumar el delito de agresión sexual, se someta a otra persona a un acto que tienda a despertar, excitar o satisfacer la pasión o deseos sexuales de la persona imputada.
 ")),
 tags$li(HTML(
  "<b>Agresión Sexual: </b> El Código Penal de Puerto Rico define el delito de agresión sexual como llevar a cabo, o provocar que otra persona lleve a cabo, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) en cualquiera de estas circunstancias:
   <ul>          
   <li>a. si a la víctima se le disminuyó, sin esta consentir o sin saberlo, su capacidad de consentir mediante algún medio hipnótico, narcótico, deprimente o estimulante.; 
 <li>b. si a la víctima se le obligó al acto por medio de fuerza física, violencia o intimidación.; 
 <li>c. si al momento del acto la víctima no tenía capacidad para consentir y la persona agresora lo sabía.; 
 <li>d. si la víctima consintió porque se le engañó sobre la identidad de la persona agresora y creía que era otra persona.; 
 <li>e. si la víctima no ha cumplido 16 años de edad.; 
 <li>f. si por enfermedad o incapacidad mental la víctima no puede comprender el acto en el momento en que ocurre.;")),
),
tags$li(HTML(
  "<b>Incesto: </b> El Código Penal de Puerto Rico define el delito de incesto como llevar a cabo a propósito, con conocimiento o temerariamente, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) con una persona con quien se tenga una relación de parentesco por cualquiera de estas circunstancias:
:
   <ul>          
   <li>a. compartir o poseer la custodia física o patria potestad.; 
 <li>b. ser ascendiente (padre, madre, abuelo, abuela, bisabuelo, bisabuela, etc.) o descendiente (hijos, hijas, nietos, nietas, bisnietos, bisnietas, etc.).; 
 <li>c. ser colateral (hermanos, hermanas, sobrinos, sobrinas, etc.) por consanguinidad o adopción, hasta el tercer grado.;
 <li>d. ser pariente por consanguinidad, adopción o afinidad.;")),
),
 
tags$li(HTML("<b>Solicitudes de órdenes de protección archivadas:</b> A solicitud de la parte peticionaria: Es aquella solicitud que, luego de que el Tribunal de Primera Instancia examina las alegaciones de la parte peticionaria para solicitar el archivo, ordena el archivo de la petición de orden de protección por un desistimiento voluntario de la parte peticionaria. ")),
tags$li(HTML("<b>Solicitudes de órdenes de protección archivadas / Otras razones:</b> Es aquella solicitud que, luego de que el Tribunal de Primera Instancia examina las alegaciones presentadas, ordena el archivo de la petición de orden de protección por incumplimiento de las órdenes del Tribunal u otras razones distintas a la petición de desistimiento de la parte peticionaria. ")),
tags$li(HTML("<b>Solicitudes de órdenes de protección denegadas:</b> Es aquella solicitud en donde el Tribunal, luego de haber escuchado a ambas partes y examinado el expediente judicial y la prueba presentada, declara No Ha Lugar la petición de orden de protección. ")),
tags$li(HTML("<b>Órdenes de protección finales:</b> Es una orden emitida por el Tribunal de Primera Instancia luego de hacer una determinación final sobre los hechos.
 ")),
tags$li(HTML("<b>Parte peticionada:</b> Persona contra quien se solicita una orden de protección. ")),
tags$li(HTML("<b>Parte peticionaria:</b> Persona que solicita una orden de protección. ")),
tags$li(HTML("<b>Sexo:</b> Indica si la persona que solicita la orden de protección, en el periodo de tiempo de interés en la región judicial especificada, se identifica como hombre o mujer.
 ")),
tags$li(HTML("<b>Tribunal de Primera Instancia:</b> Es un tribunal de jurisdicción original general con autoridad para actuar en todo tipo de procedimiento judicial, civil o criminal, bajo la Constitución y las leyes del Estado Libre Asociado de Puerto Rico.
 ")),
tags$li(HTML("<b>Violación orden de protección Art 2.8:</b> Incumplimiento de órdenes de protección. Cualquier violación a sabiendas de una orden de protección expedida. ")),
tags$li(HTML("<b>Maltrato Art 3.1:</b> Toda persona que empleare fuerza física o violencia psicológica, intimidación o persecución en la persona de su cónyuge, ex cónyuge, o la persona con quien cohabita o haya cohabitado, o la persona con quien sostuviere o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, para causarle daño físico a su persona, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro o para causarle grave daño emocional, incurrirá en delito grave de cuarto grado en su mitad superior.
 ")),
 tags$li(HTML(
  "<b>Maltrato agravado Art. 3.2: </b> Se impondrá pena correspondiente a delito grave de tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando una o más de las circunstancias siguientes:
   <ul>          
   <li>a. Se penetrare en la morada de la persona o en el lugar donde esté albergada y se cometiere allí maltrato, en el caso de cónyuges o cohabitantes, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, cuando éstos estuvieren separados o mediare una orden de protección ordenando el desalojo de la residencia a una de las partes; 
 <li>b. cuando se infiere grave daño corporal a la persona; 
 <li>c. cuando se cometiere con arma mortífera en circunstancias que no revistiesen la intención de matar o mutilar; 
 <li>d. cuando se cometiere en la presencia de menores de edad;
 <li>e. cuando se cometiere luego de mediar una orden de protección o resolución contra la persona acusada expedida en auxilio de la víctima del maltrato;
 <li>f. se indujere, incitare u obligare a la persona a drogarse con sustancias controladas, o cualquier otra sustancia o medio que altere la voluntad de la persona o a intoxicarse con bebidas embriagantes;
 <li>b. cuando se cometiere y simultáneamente se incurriere en maltrato de un menor según definido en la Ley Núm. 177 de 1 de Agosto de 2003 [Nota: Derogada y sustituida por la Ley 246- 2011, según enmendada, “Ley para la Seguridad, Bienestar y Protección de Menores”];
 <li>. si a la víctima se le obliga o induce mediante maltrato, violencia física o sicológica a participar o involucrarse en una relación sexual no deseada con terceras personas. (i) Cuando se cometiere contra una mujer embarazada.;")),
tags$li(HTML("<b>Maltrato mediante amenaza Art 3.3:</b> Toda persona que amenazare con causarle daño a su cónyuge, ex cónyuge, a la persona con quien cohabita o con quien haya cohabitado o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro, incurrirá en delito grave de cuarto grado en su mitad superior. ")),
tags$li(HTML("<b>Maltrato mediante restricción de libertad Art. 3.4:</b> Toda persona que utilice violencia o intimidación en la persona de su cónyuge, ex cónyuge, de la persona con quien cohabita o haya cohabitado, o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, o que utilice pretexto de que padece o de que una de las personas antes mencionadas padece de enfermedad o defecto mental, para restringir su libertad con el conocimiento de la víctima, incurrirá en delito grave de tercer grado en su mitad inferior. ")),
tags$li(HTML(
  "<b>Agresión Sexual Conyugal (Artículo 3.5): </b> Se impondrá pena de reclusión, según se dispone más adelante, a toda persona que incurra en una relación sexual no consentida con su cónyuge o ex cónyuge, o con la persona con quien cohabite o haya cohabitado, o con quien sostuviere o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, en cualesquiera de las circunstancias siguientes:
   <ul>          
   <li>a. Si se ha compelido a incurrir en relación sexual mediante el empleo de fuerza, violencia, intimidación o amenaza de grave e inmediato daño corporal; 
 <li>b. Si se ha anulado o disminuido sustancialmente, sin su conocimiento o sin su consentimiento, su capacidad de consentir, a través de medios hipnóticos, narcóticos, deprimentes o estimulantes o sustancias o medios similares; 
 <li>c. Si por enfermedad o incapacidad mental, temporal o permanente, la víctima está incapacitada para comprender la naturaleza del acto en el momento de su realización; 
 <li>d. Si se le obliga o induce mediante maltrato, violencia física o psicológica a participar o involucrarse en una relación sexual no deseada con terceras personas.;")),
),
tags$li(HTML("<b>Violación orden de protección Art 2.8:</b> Incumplimiento de órdenes de protección. Cualquier violación a sabiendas de una orden de protección expedida. ")),
tags$li(HTML(
  "<b>Acoso Sexual: </b> El Código Penal de Puerto Rico establece que el delito de acoso sexual se configura en una relación laboral, docente o de prestación de servicios, e implica lo siguiente:
   <ul>          
   <li>a. solicitar favores de naturaleza sexual para sí o para un tercero y sujetar las condiciones de trabajo, docencia o servicios a su cumplimiento.; 
 <li>b. mediante comportamiento sexual, provocar una situación con conocimiento de que resultará intimidatoria, hostil o humillante para la víctima.
;")),
),
tags$li(HTML("<b>Actos Lascivos:</b> El Código Penal de Puerto Rico define el delito de actos lascivos como aquel en el cual, sin intentar consumar el delito de agresión sexual, se someta a otra persona a un acto que tienda a despertar, excitar o satisfacer la pasión o deseos sexuales de la persona imputada.
 ")),
 tags$li(HTML(
  "<b>Agresión Sexual: </b> El Código Penal de Puerto Rico define el delito de agresión sexual como llevar a cabo, o provocar que otra persona lleve a cabo, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) en cualquiera de estas circunstancias:
   <ul>          
   <li>a. si a la víctima se le disminuyó, sin esta consentir o sin saberlo, su capacidad de consentir mediante algún medio hipnótico, narcótico, deprimente o estimulante.; 
 <li>b. si a la víctima se le obligó al acto por medio de fuerza física, violencia o intimidación.; 
 <li>c. si al momento del acto la víctima no tenía capacidad para consentir y la persona agresora lo sabía.; 
 <li>d. si la víctima consintió porque se le engañó sobre la identidad de la persona agresora y creía que era otra persona.; 
 <li>e. si la víctima no ha cumplido 16 años de edad.; 
 <li>f. si por enfermedad o incapacidad mental la víctima no puede comprender el acto en el momento en que ocurre.;")),
),
tags$li(HTML(
  "<b>Incesto: </b> El Código Penal de Puerto Rico define el delito de incesto como llevar a cabo a propósito, con conocimiento o temerariamente, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) con una persona con quien se tenga una relación de parentesco por cualquiera de estas circunstancias:
:
   <ul>          
   <li>a. compartir o poseer la custodia física o patria potestad.; 
 <li>b. ser ascendiente (padre, madre, abuelo, abuela, bisabuelo, bisabuela, etc.) o descendiente (hijos, hijas, nietos, nietas, bisnietos, bisnietas, etc.).; 
 <li>c. ser colateral (hermanos, hermanas, sobrinos, sobrinas, etc.) por consanguinidad o adopción, hasta el tercer grado.;
 <li>d. ser pariente por consanguinidad, adopción o afinidad.;")),
),
tags$li(HTML("<b>Solicitudes de órdenes de protección archivadas:</b> A solicitud de la parte peticionaria: Es aquella solicitud que, luego de que el Tribunal de Primera Instancia examina las alegaciones de la parte peticionaria para solicitar el archivo, ordena el archivo de la petición de orden de protección por un desistimiento voluntario de la parte peticionaria. ")),
tags$li(HTML("<b>Solicitudes de órdenes de protección archivadas / Otras razones:</b> Es aquella solicitud que, luego de que el Tribunal de Primera Instancia examina las alegaciones presentadas, ordena el archivo de la petición de orden de protección por incumplimiento de las órdenes del Tribunal u otras razones distintas a la petición de desistimiento de la parte peticionaria. ")),
tags$li(HTML("<b>Solicitudes de órdenes de protección denegadas:</b> Es aquella solicitud en donde el Tribunal, luego de haber escuchado a ambas partes y examinado el expediente judicial y la prueba presentada, declara No Ha Lugar la petición de orden de protección. ")),
tags$li(HTML("<b>Órdenes de protección finales:</b> Es una orden emitida por el Tribunal de Primera Instancia luego de hacer una determinación final sobre los hechos.
 ")),
tags$li(HTML("<b>Parte peticionada:</b> Persona contra quien se solicita una orden de protección. ")),
tags$li(HTML("<b>Parte peticionaria:</b> Persona que solicita una orden de protección. ")),
tags$li(HTML("<b>Sexo:</b> Indica si la persona que solicita la orden de protección, en el periodo de tiempo de interés en la región judicial especificada, se identifica como hombre o mujer.
 ")),
tags$li(HTML("<b>Tribunal de Primera Instancia:</b> Es un tribunal de jurisdicción original general con autoridad para actuar en todo tipo de procedimiento judicial, civil o criminal, bajo la Constitución y las leyes del Estado Libre Asociado de Puerto Rico.
 ")),
tags$li(HTML("<b>Violación orden de protección Art 2.8:</b> Incumplimiento de órdenes de protección. Cualquier violación a sabiendas de una orden de protección expedida. ")),
tags$li(HTML("<b>Maltrato Art 3.1:</b> Toda persona que empleare fuerza física o violencia psicológica, intimidación o persecución en la persona de su cónyuge, ex cónyuge, o la persona con quien cohabita o haya cohabitado, o la persona con quien sostuviere o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, para causarle daño físico a su persona, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro o para causarle grave daño emocional, incurrirá en delito grave de cuarto grado en su mitad superior.
 ")),
 tags$li(HTML(
  "<b>Maltrato agravado Art. 3.2: </b> Se impondrá pena correspondiente a delito grave de tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando una o más de las circunstancias siguientes:
   <ul>          
   <li>a. Se penetrare en la morada de la persona o en el lugar donde esté albergada y se cometiere allí maltrato, en el caso de cónyuges o cohabitantes, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, cuando éstos estuvieren separados o mediare una orden de protección ordenando el desalojo de la residencia a una de las partes; 
 <li>b. cuando se infiere grave daño corporal a la persona; 
 <li>c. cuando se cometiere con arma mortífera en circunstancias que no revistiesen la intención de matar o mutilar; 
 <li>d. cuando se cometiere en la presencia de menores de edad;
 <li>e. cuando se cometiere luego de mediar una orden de protección o resolución contra la persona acusada expedida en auxilio de la víctima del maltrato;
 <li>f. se indujere, incitare u obligare a la persona a drogarse con sustancias controladas, o cualquier otra sustancia o medio que altere la voluntad de la persona o a intoxicarse con bebidas embriagantes;
 <li>b. cuando se cometiere y simultáneamente se incurriere en maltrato de un menor según definido en la Ley Núm. 177 de 1 de Agosto de 2003 [Nota: Derogada y sustituida por la Ley 246- 2011, según enmendada, “Ley para la Seguridad, Bienestar y Protección de Menores”];
 <li>. si a la víctima se le obliga o induce mediante maltrato, violencia física o sicológica a participar o involucrarse en una relación sexual no deseada con terceras personas. (i) Cuando se cometiere contra una mujer embarazada.;")),
tags$li(HTML("<b>Maltrato mediante amenaza Art 3.3:</b> Toda persona que amenazare con causarle daño a su cónyuge, ex cónyuge, a la persona con quien cohabita o con quien haya cohabitado o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro, incurrirá en delito grave de cuarto grado en su mitad superior. ")),
tags$li(HTML("<b>Maltrato mediante restricción de libertad Art. 3.4:</b> Toda persona que utilice violencia o intimidación en la persona de su cónyuge, ex cónyuge, de la persona con quien cohabita o haya cohabitado, o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, o que utilice pretexto de que padece o de que una de las personas antes mencionadas padece de enfermedad o defecto mental, para restringir su libertad con el conocimiento de la víctima, incurrirá en delito grave de tercer grado en su mitad inferior. ")),
tags$li(HTML(
  "<b>Agresión Sexual Conyugal (Artículo 3.5): </b> Se impondrá pena de reclusión, según se dispone más adelante, a toda persona que incurra en una relación sexual no consentida con su cónyuge o ex cónyuge, o con la persona con quien cohabite o haya cohabitado, o con quien sostuviere o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, en cualesquiera de las circunstancias siguientes:
   <ul>          
   <li>a. Si se ha compelido a incurrir en relación sexual mediante el empleo de fuerza, violencia, intimidación o amenaza de grave e inmediato daño corporal; 
 <li>b. Si se ha anulado o disminuido sustancialmente, sin su conocimiento o sin su consentimiento, su capacidad de consentir, a través de medios hipnóticos, narcóticos, deprimentes o estimulantes o sustancias o medios similares; 
 <li>c. Si por enfermedad o incapacidad mental, temporal o permanente, la víctima está incapacitada para comprender la naturaleza del acto en el momento de su realización; 
 <li>d. Si se le obliga o induce mediante maltrato, violencia física o psicológica a participar o involucrarse en una relación sexual no deseada con terceras personas.;")),
),
tags$li(HTML("<b>Violación orden de protección Art 2.8:</b> Incumplimiento de órdenes de protección. Cualquier violación a sabiendas de una orden de protección expedida. ")),














tags$li(HTML("<b>:</b> ")),
tags$li(HTML("<b>:</b> ")),