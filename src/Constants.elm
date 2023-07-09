module Constants exposing (apiUrlCrosswords)


apiUrl : String
apiUrl =
    "http://localhost:8080"


apiUrlCrosswords : String
apiUrlCrosswords =
    String.concat [ apiUrl, "/crosswords" ]
