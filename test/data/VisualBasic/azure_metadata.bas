'  Azure Blob Service Example: Set Container User-Defined Metadata
'  See also: https://msdn.microsoft.com/en-us/library/azure/dd179362.aspx

'  This example requires the Chilkat API to have been previously unlocked.
'  See Global Unlock Sample for sample code.

Dim rest As New ChilkatRest

'  Connect to the Azure Storage Blob Service
Dim bTls As Long
bTls = 1
Dim port As Long
port = 443
Dim bAutoReconnect As Long
bAutoReconnect = 1
'  In this example, the storage account name is "chilkat".
Dim success As Long
success = rest.Connect("chilkat.blob.core.windows.net",port,bTls,bAutoReconnect)
If (success <> 1) Then
    Debug.Print rest.LastErrorText
    Exit Sub
End If

'  Provide Azure Cloud credentials for the REST call.
Dim azAuth As New ChilkatAuthAzureStorage
azAuth.AccessKey = "AZURE_ACCESS_KEY"
'  The account name used here should match the 1st part of the domain passed in the call to Connect (above).
azAuth.Account = "chilkat"
azAuth.Scheme = "SharedKey"
azAuth.Service = "Blob"
'  This causes the "x-ms-version: 2015-02-21" header to be automatically added.
azAuth.XMsVersion = "2015-02-21"
success = rest.SetAuthAzureStorage(azAuth)

'  Note: The application does not need to explicitly set the following
'  headers: x-ms-date, Authorization.  These headers
'  are automatically set by Chilkat.

'  Add each name-value pair of metadata to associate with the container.
'  To remove all metadata from the container, simply don't add any x-ms-meta- headers.
success = rest.AddHeader("x-ms-meta-Category","Images")
success = rest.AddHeader("x-ms-meta-Resolution","High")

'  The expected success response is a 200 response status code with no response body.
'  In this example, we are setting the metadata of the container named "mycontainer".
Dim responseStr As String
responseStr = rest.FullRequestNoBody("PUT","/mycontainer?restype=container&comp=metadata")
If (rest.LastMethodSuccess <> 1) Then
    Debug.Print rest.LastErrorText
    Exit Sub
End If

'  When successful, the Azure Storage service will respond with a 200 response status code,
'  with no response body.

If (rest.ResponseStatusCode <> 200) Then
    '  Examine the request/response to see what happened.
    Debug.Print "response status code = " & rest.ResponseStatusCode
    Debug.Print "response status text = " & rest.ResponseStatusText
    Debug.Print "response header: " & rest.ResponseHeader
    Debug.Print "response body (if any): " & responseStr
    Debug.Print "---"
    Debug.Print "LastRequestStartLine: " & rest.LastRequestStartLine
    Debug.Print "LastRequestHeader: " & rest.LastRequestHeader
    Exit Sub
End If

Debug.Print "Success."
