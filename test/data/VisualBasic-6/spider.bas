Dim spider As New Spider

'  The spider object crawls a single web site at a time.  As you'll see
'  in later examples, you can collect outbound links and use them to
'  crawl the web.  For now, we'll simply spider 10 pages of chilkatsoft.com
spider.Initialize "www.chilkatsoft.com"

'  Add the 1st URL:
spider.AddUnspidered "http://www.chilkatsoft.com/"

'  Begin crawling the site by calling CrawlNext repeatedly.
Dim i As Long
For i = 0 To 9
    Dim success As Long
    success = spider.CrawlNext()
    If (success = 1) Then
        '  Show the URL of the page just spidered.
        Debug.Print spider.LastUrl

        '  The HTML META keywords, title, and description are available in these properties:
        Debug.Print spider.LastHtmlTitle
        Debug.Print spider.LastHtmlDescription
        Debug.Print spider.LastHtmlKeywords

        '  The HTML is available in the LastHtml property
    Else
        '  Did we get an error or are there no more URLs to crawl?
        If (spider.NumUnspidered = 0) Then
            Debug.Print "No more URLs to spider"
        Else
            Debug.Print spider.LastErrorText
        End If

    End If

    '  Sleep 1 second before spidering the next URL.
    spider.SleepMs 1000
Next
