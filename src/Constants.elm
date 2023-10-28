module Constants exposing (apiUrlCrosswords)


apiUrl : String
apiUrl =
    "https://cooperative-crosswords.onrender.com"


apiUrlCrosswords : String
apiUrlCrosswords =
    String.concat [ apiUrl, "/crosswords" ]
