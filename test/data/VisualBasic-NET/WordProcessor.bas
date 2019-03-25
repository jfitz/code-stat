Imports System.Drawing
Imports System.Drawing.Image

Public Class frmMain

#Region "Declarations"
 
    Private currentFile As String
    Private checkPrint As Integer

#End Region

Private Sub NewToolStripMenuItem_Click(ByVal sender As _
        System.Object, ByVal e As System.EventArgs) _
        Handles NewToolStripMenuItem.Click

    If rtbDoc.Modified Then
        Dim answer As Integer
        answer = MessageBox.Show("The current document has not" & _
                 " been saved, would you like to " & _
                 "continue without saving?", "Unsaved Document", _
                 MessageBoxButtons.YesNo, MessageBoxIcon.Question)

        If answer = Windows.Forms.DialogResult.Yes Then
            rtbDoc.Clear()
        Else
            Exit Sub
        End If
    Else
        rtbDoc.Clear()
    End If

    currentFile = ""
    Me.Text = "Editor: New Document"
End Sub

Private Sub OpenToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles OpenToolStripMenuItem.Click

    If rtbDoc.Modified Then

        Dim answer As Integer
        answer = MessageBox.Show("The current document has not" & _
                 " been saved, would you like to continue " & _
                 "without saving?", "Unsaved Document", _
                 MessageBoxButtons.YesNo, MessageBoxIcon.Question)

        If answer = Windows.Forms.DialogResult.No Then
            Exit Sub
        Else
            OpenFile()
        End If
    Else
        OpenFile()
    End If

End Sub

Private Sub OpenFile()

    OpenFileDialog1.Title = "RTE - Open File"
    OpenFileDialog1.DefaultExt = "rtf"
    OpenFileDialog1.Filter = "Rich Text Files|*.rtf|" & _
                             "Text Files|*.txt|HTML Files|" & _
                             "*.htm|All Files|*.*"
    OpenFileDialog1.FilterIndex = 1
    OpenFileDialog1.ShowDialog()

    If OpenFileDialog1.FileName = "" Then Exit Sub

    Dim strExt As String
    strExt = System.IO.Path.GetExtension(OpenFileDialog1.FileName)
    strExt = strExt.ToUpper()

    Select Case strExt
        Case ".RTF"
            rtbDoc.LoadFile(OpenFileDialog1.FileName,
            RichTextBoxStreamType.RichText)
        Case Else
            Dim txtReader As System.IO.StreamReader
            txtReader = New
            System.IO.StreamReader(OpenFileDialog1.FileName)
            rtbDoc.Text = txtReader.ReadToEnd
            txtReader.Close()
            txtReader = Nothing
            rtbDoc.SelectionStart = 0
            rtbDoc.SelectionLength = 0
    End Select

    currentFile = OpenFileDialog1.FileName
    rtbDoc.Modified = False
    Me.Text = "Editor: " & currentFile.ToString()

End Sub

Private Sub SaveToolStripMenuItem_Click(ByVal sender As _
        System.Object, ByVal e As System.EventArgs) _
        Handles SaveToolStripMenuItem.Click

    If currentFile = "" Then
        SaveAsToolStripMenuItem_Click(Me, e)
        Exit Sub
    End If

    Dim strExt As String
    strExt = System.IO.Path.GetExtension(currentFile)
    strExt = strExt.ToUpper()

    Select Case strExt
        Case ".RTF"
            rtbDoc.SaveFile(currentFile)
        Case Else
            ' to save as plain text
            Dim txtWriter As System.IO.StreamWriter
            txtWriter = New System.IO.StreamWriter(currentFile)
            txtWriter.Write(rtbDoc.Text)
            txtWriter.Close()
            txtWriter = Nothing
            rtbDoc.SelectionStart = 0
            rtbDoc.SelectionLength = 0
            rtbDoc.Modified = False
    End Select

    Me.Text = "Editor: " & currentFile.ToString()

End Sub

Private Sub SaveAsToolStripMenuItem_Click(ByVal sender As _
        System.Object, ByVal e As System.EventArgs) _
        Handles SaveAsToolStripMenuItem.Click

    SaveFileDialog1.Title = "RTE - Save File"
    SaveFileDialog1.DefaultExt = "rtf"
    SaveFileDialog1.Filter = "Rich Text Files|*.rtf|" & _
                             "Text Files|*.txt|HTML Files" & _
                             "|*.htm|All Files|*.*"
    SaveFileDialog1.FilterIndex = 1
    SaveFileDialog1.ShowDialog()

    If SaveFileDialog1.FileName = "" Then Exit Sub

    Dim strExt As String
    strExt = System.IO.Path.GetExtension(SaveFileDialog1.FileName)
    strExt = strExt.ToUpper()

    Select Case strExt
        Case ".RTF"
            rtbDoc.SaveFile(SaveFileDialog1.FileName, _
                   RichTextBoxStreamType.RichText)
        Case Else
            Dim txtWriter As System.IO.StreamWriter
            txtWriter = New
            System.IO.StreamWriter(SaveFileDialog1.FileName)
            txtWriter.Write(rtbDoc.Text)
            txtWriter.Close()
            txtWriter = Nothing
            rtbDoc.SelectionStart = 0
            rtbDoc.SelectionLength = 0
    End Select

    currentFile = SaveFileDialog1.FileName
    rtbDoc.Modified = False
    Me.Text = "Editor: " & currentFile.ToString()

End Sub

Private Sub ExitToolStripMenuItem_Click(ByVal sender As _
        System.Object, ByVal e As System.EventArgs) _
        Handles ExitToolStripMenuItem.Click

    If rtbDoc.Modified Then

        Dim answer As Integer
        answer = MessageBox.Show("The current document has not been" & _
                 " saved, would you like to continue without saving?", _
                 "Unsaved Document", MessageBoxButtons.YesNo, _
                 MessageBoxIcon.Question)

        If answer = Windows.Forms.DialogResult.No Then
            Exit Sub
        Else
            Application.Exit()
        End If
    Else
        Application.Exit()
    End If

End Sub

Private Sub SelectAllToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles SelectAllToolStripMenuItem.Click

    Try
        rtbDoc.SelectAll()
    Catch exc As Exception
        MessageBox.Show("Unable to select all document content.", _
                        "RTE – Select", MessageBoxButtons.OK, _
                        MessageBoxIcon.Error)
    End Try

End Sub

Private Sub SelectFontToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles SelectFontToolStripMenuItem.Click

    If Not rtbDoc.SelectionFont Is Nothing Then
        FontDialog1.Font = rtbDoc.SelectionFont
    Else
        FontDialog1.Font = Nothing
    End If

    FontDialog1.ShowApply = True
    If FontDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
        rtbDoc.SelectionFont = FontDialog1.Font
    End If

End Sub

Private Sub BoldToolStripMenuItem_Click(ByVal sender As _
       System.Object, ByVal e As System.EventArgs) _
       Handles BoldToolStripMenuItem.Click
    If Not rtbDoc.SelectionFont Is Nothing Then
        Dim currentFont As System.Drawing.Font = rtbDoc.SelectionFont
        Dim newFontStyle As System.Drawing.FontStyle
        If rtbDoc.SelectionFont.Bold = True Then
            newFontStyle = FontStyle.Regular
        Else
            newFontStyle = FontStyle.Bold
        End If

        rtbDoc.SelectionFont = New Font(currentFont.FontFamily, _
                               currentFont.Size, newFontStyle)
    End If
End Sub

Private Sub NormalToolStripMenuItem_Click(ByVal sender As _
        System.Object, ByVal e As System.EventArgs) _
        Handles NormalToolStripMenuItem.Click
    If Not rtbDoc.SelectionFont Is Nothing Then
        Dim currentFont As System.Drawing.Font = rtbDoc.SelectionFont
        Dim newFontStyle As System.Drawing.FontStyle
        newFontStyle = FontStyle.Regular

        rtbDoc.SelectionFont = New Font(currentFont.FontFamily, _
                               currentFont.Size, newFontStyle)
    End If
End Sub

Private Sub PageColorToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles PageColorToolStripMenuItem.Click
    ColorDialog1.Color = rtbDoc.BackColor
    If ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
        rtbDoc.BackColor = ColorDialog1.Color
    End If
End Sub

Private Sub mnuUndo_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles mnuUndo.Click
    If rtbDoc.CanUndo Then rtbDoc.Undo()
End Sub

Private Sub mnuRedo_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles mnuRedo.Click
    If rtbDoc.CanRedo Then rtbDoc.Redo()
End Sub

Private Sub LeftToolStripMenuItem_Click_1(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles LeftToolStripMenuItem.Click
    rtbDoc.SelectionAlignment = HorizontalAlignment.Left
End Sub

Private Sub CenterToolStripMenuItem_Click_1(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles CenterToolStripMenuItem.Click
    rtbDoc.SelectionAlignment = HorizontalAlignment.Center
End Sub

Private Sub RightToolStripMenuItem_Click_1(ByVal sender As _
        System.Object, ByVal e As System.EventArgs) _
        Handles RightToolStripMenuItem.Click
    rtbDoc.SelectionAlignment = HorizontalAlignment.Right
End Sub

Private Sub AddBulletsToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles AddBulletsToolStripMenuItem.Click
    rtbDoc.BulletIndent = 10
    rtbDoc.SelectionBullet = True
End Sub

Private Sub RemoveBulletsToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles RemoveBulletsToolStripMenuItem.Click
    rtbDoc.SelectionBullet = False
End Sub

Private Sub mnuIndent0_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles mnuIndent0.Click
    rtbDoc.SelectionIndent = 0
End Sub

Private Sub FindToolStripMenuItem_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles FindToolStripMenuItem.Click
    Dim f As New frmFind()
    f.Show()
End Sub

Private Sub FindAndReplaceToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles FindAndReplaceToolStripMenuItem.Click
    Dim f As New frmReplace()
    f.Show()
End Sub

Private Sub PreviewToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles PreviewToolStripMenuItem.Click
    PrintPreviewDialog1.Document = PrintDocument1
    PrintPreviewDialog1.ShowDialog()
End Sub

Private Sub PrintToolStripMenuItem_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) _
        Handles PrintToolStripMenuItem.Click
    PrintDialog1.Document = PrintDocument1
    If PrintDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
        PrintDocument1.Print()
    End If
End Sub

Private Sub mnuPageSetup_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles mnuPageSetup.Click
    PageSetupDialog1.Document = PrintDocument1
    PageSetupDialog1.ShowDialog()
End Sub

Private Sub InsertImageToolStripMenuItem_Click(ByVal sender _
        As System.Object, ByVal e As System.EventArgs) _
        Handles InsertImageToolStripMenuItem.Click

    OpenFileDialog1.Title = "RTE - Insert Image File"
    OpenFileDialog1.DefaultExt = "rtf"
    OpenFileDialog1.Filter = "Bitmap Files|*.bmp|JPEG Files|*.jpg|GIF Files|*.gif"
    OpenFileDialog1.FilterIndex = 1
    OpenFileDialog1.ShowDialog()

    If OpenFileDialog1.FileName = "" Then Exit Sub

    Try
        Dim strImagePath As String = OpenFileDialog1.FileName
        Dim img As Image
        img = Image.FromFile(strImagePath)
        Clipboard.SetDataObject(img)
        Dim df As DataFormats.Format
        df = DataFormats.GetFormat(DataFormats.Bitmap)
        If Me.rtbDoc.CanPaste(df) Then
            Me.rtbDoc.Paste(df)
        End If
    Catch ex As Exception
        MessageBox.Show("Unable to insert image format selected.", "RTE – Paste", MessageBoxButtons.OK, MessageBoxIcon.Error)
    End Try

End Sub

Private Sub PrintDocument1_BeginPrint(ByVal sender As Object, _
        ByVal e As System.Drawing.Printing.PrintEventArgs) _
        Handles PrintDocument1.BeginPrint
    ' Adapted from Microsoft's example for extended richtextbox control
    '
    checkPrint = 0
End Sub

Private Sub PrintDocument1_PrintPage(ByVal sender As Object, _
        ByVal e As System.Drawing.Printing.PrintPageEventArgs) _
        Handles PrintDocument1.PrintPage

    ' Adapted from Microsoft's example for extended richtextbox control
    '
    ' Print the content of the RichTextBox. Store the last character
      printed.
    checkPrint = rtbDoc.Print(checkPrint, rtbDoc.TextLength, e)

    ' Look for more pages
    If checkPrint < rtbDoc.TextLength Then
        e.HasMorePages = True
    Else
        e.HasMorePages = False
    End If

End Sub

Private Sub btnFind_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles btnFind.Click

    Dim StartPosition As Integer
    Dim SearchType As CompareMethod

    If chkMatchCase.Checked = True Then
        SearchType = CompareMethod.Binary
    Else
        SearchType = CompareMethod.Text
    End If

    StartPosition = InStr(1, frmMain.rtbDoc.Text, _
                     txtSearchTerm.Text, SearchType)

    If StartPosition = 0 Then
        MessageBox.Show("String: '" & txtSearchTerm.Text.ToString() & _
                        "' not found", "No Matches", _
                        MessageBoxButtons.OK, MessageBoxIcon.Asterisk)
        Exit Sub
    End If

    frmMain.rtbDoc.Select(StartPosition - 1, txtSearchTerm.Text.Length)
    frmMain.rtbDoc.ScrollToCaret()
    frmMain.Focus()

End Sub

Private Sub btnFindNext_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles btnFindNext.Click

    Dim StartPosition As Integer = frmMain.rtbDoc.SelectionStart + 2
    Dim SearchType As CompareMethod

    If chkMatchCase.Checked = True Then
        SearchType = CompareMethod.Binary
    Else
        SearchType = CompareMethod.Text
    End If

    StartPosition = InStr(StartPosition, frmMain.rtbDoc.Text, _
                    txtSearchTerm.Text, SearchType)

    If StartPosition = 0 Then
        MessageBox.Show("String: '" & txtSearchTerm.Text.ToString() & _
                        "' not found", "No Matches", _
                        MessageBoxButtons.OK, MessageBoxIcon.Asterisk)
        Exit Sub
    End If

    frmMain.rtbDoc.Select(StartPosition - 1, txtSearchTerm.Text.Length)
    frmMain.rtbDoc.ScrollToCaret()
    frmMain.Focus()

End Sub

Private Sub btnReplace_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles btnReplace.Click

    If frmMain.rtbDoc.SelectedText.Length <> 0 Then
        frmMain.rtbDoc.SelectedText = txtReplacementText.Text
    End If

    Dim StartPosition As Integer = frmMain.rtbDoc.SelectionStart + 2
    Dim SearchType As CompareMethod

    If chkMatchCase.Checked = True Then
        SearchType = CompareMethod.Binary
    Else
        SearchType = CompareMethod.Text
    End If

    StartPosition = InStr(StartPosition, frmMain.rtbDoc.Text,_
                    txtSearchTerm.Text, SearchType)

    If StartPosition = 0 Then
        MessageBox.Show("String: '" & txtSearchTerm.Text.ToString() & _
                        "' not found", "No Matches", _
                        MessageBoxButtons.OK, MessageBoxIcon.Asterisk)
        Exit Sub
    End If

    frmMain.rtbDoc.Select(StartPosition - 1, txtSearchTerm.Text.Length)
    frmMain.rtbDoc.ScrollToCaret()
    frmMain.Focus()

End Sub

Private Sub btnReplaceAll_Click(ByVal sender As System.Object, _
        ByVal e As System.EventArgs) Handles btnReplaceAll.Click

    Dim currentPosition As Integer = frmMain.rtbDoc.SelectionStart
    Dim currentSelect As Integer = frmMain.rtbDoc.SelectionLength

    frmMain.rtbDoc.Rtf = Replace(frmMain.rtbDoc.Rtf, _
                         Trim(txtSearchTerm.Text), _
                         Trim(txtReplacementText.Text))
    frmMain.rtbDoc.SelectionStart = currentPosition
    frmMain.rtbDoc.SelectionLength = currentSelect
    frmMain.Focus()

End Sub
