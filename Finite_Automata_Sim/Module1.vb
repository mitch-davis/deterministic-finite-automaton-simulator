Imports System
Imports System.IO


Module Module1

    Public Enum Machine_Type
        NFA 'Non-Deterministic Finite Automaton
        DFA 'Deterministic Finite Automaton
        INVALID
    End Enum

    'Provides functionality for a Deterministic Finite Automaton; can identify NFA's or invalid FA's
    Public Class Finite_Automaton
        Dim Q As New HashSet(Of String) 'set of states
        Dim E As New HashSet(Of String) 'alphabet
        Dim q0 As String = "0" 'start state
        Dim d As New Dictionary(Of String, Dictionary(Of String, String)) 'transition function in form (current state) -> (input character) -> (resultant state) for simpler machine execution
        Dim F As String() 'set of accept states

        Dim rules(0)() As String  'raw transistion rules in form (current State, input character, resultant state) for simpler validation 
        Dim currentState As String = q0
        Dim machineType As Machine_Type

        Public Sub SetMachineType(type As Machine_Type)
            machineType = type
        End Sub

        'Resets the machine to a blank slate
        Public Sub Clear()
            Q.Clear()
            E.Clear()
            d.Clear()
            ReDim F(0)
            ReDim rules(0)
            currentState = q0
        End Sub

        'Load the core components of the machine from the FA description file
        Public Sub Initialize(file As Integer)
            Dim rule As String 'a single rule
            Dim ruleSplit() As String 'a single rule split into (current state, input character, resultant state)
            Dim first As Boolean = True 'first rule is added to array slightly different

            Dim temp As String = LineInput(file)
            temp = temp.Trim({"{"c, "}"c})
            F = temp.Split(",") 'load the set of accept states

            For Each state As String In F
                Q.Add(state)
            Next

            Do While Not EOF(file)
                rule = LineInput(file) 'get next rule
                'load rule into the rules array
                If first Then
                    rules(0) = rule.Split(",")
                    first = False
                Else
                    ReDim Preserve rules(rules.Length)
                    rules(rules.Length - 1) = rule.Split(",")
                End If

                ruleSplit = rule.Split(",")

                'initialize the keys for each state (nested dictionary will be loaded later if DFA is verified)
                If d.ContainsKey(ruleSplit(0)) Then
                    'state already enumerated
                Else
                    d.Add(ruleSplit(0), New Dictionary(Of String, String))
                End If

                'Load the set of states
                If Q.Contains(ruleSplit(0)) Then
                    'state already enumerated
                Else
                    Q.Add(ruleSplit(0))
                End If

                'load the alphabet
                If E.Contains(ruleSplit(1)) Then
                    'character already enumerated
                Else
                    E.Add(ruleSplit(1))
                End If
            Loop

        End Sub

        'Checks various conditions to ensure the machine is a valid FA; if it is, determine if it is an NFA or a DFA
        Public Function ValidateMachine() As Machine_Type
            'Check validity -
            'Verify at least one Accept State and all are valid states
            If F.Length = 0 Then
                Return Machine_Type.INVALID
            Else
                For Each state As String In F
                    If Not Q.Contains(state) Then
                        Return Machine_Type.INVALID
                    End If
                Next
            End If

            'Verify rules use valid input characters and states
            For Each rule As String() In rules
                If Not E.Contains(rule(1)) Or Not Q.Contains(rule(0)) Or Not Q.Contains(rule(2)) Then
                    Return Machine_Type.INVALID
                End If
            Next

            'Check determinism -
            'epsilon implies NFA
            If E.Contains("") Then
                Return Machine_Type.NFA
            End If

            'verify only one transition for each input, else implies NFA
            Dim numTransitions As Integer
            For Each character As String In E
                numTransitions = 0
                For Each rule As String() In rules
                    If rule(1) = character Then
                        numTransitions += 1
                    End If
                Next
                'number of transistions using this character should match number of states if DFA
                If numTransitions = Q.Count Then
                    'DFA
                Else
                    Return Machine_Type.NFA
                End If
            Next

            'If all validations pass, then the machine is Deterministic; formal transition function can now be loaded
            For Each rule As String() In rules
                d(rule(0)).Add(rule(1), rule(2))
            Next
            Return Machine_Type.DFA

        End Function

        'Takes a string and returns true if it is accepted by the machine, and false if it is rejected or not a valid input string
        Public Function EvaluateString(word As String) As Boolean
            Dim i As Integer
            currentState = q0 'go to start state

            If Not ValidString(word) Then
                Return False 'Not a valid input string (has characters which aren't in the input alphabet)
            End If

            For i = 0 To word.Length - 1
                currentState = Delta(word(i)) 'Evaluate Delta Function for each character in the string
            Next

            For Each state In F
                If currentState = state Then
                    Return True 'In accept state -> Accept
                End If
            Next

            Return False 'Not in accept state -> Reject

        End Function

        'Evaluates a single transition and returns the resultant state
        Public Function Delta(chr As String) As String
            Return d(currentState)(chr) 'look up the new state via current state and input character
        End Function

        'Prints the input alphabet to the log file
        Public Sub PrintAlphabet(file As Integer)
            Print(file, "Alphabet: [")
            For Each chr As String In E
                Print(file, chr)
            Next
            Print(file, "]" + vbNewLine)
        End Sub

        'Prints the number of states (|Q|) to the log file
        Public Sub PrintNumStates(file As Integer)
            Print(file, "States: " + Q.Count.ToString() + vbNewLine)
        End Sub

        'Checks that the input string is valid for the current Automaton (contains only characters from the input alphabet)
        Private Function ValidString(word As String)
            Dim i As Integer
            For i = 0 To word.Length - 1
                If Not E.Contains(word(i)) Then
                    Return False 'If ANY character is not in the input alphabet, not a valid string
                End If
            Next
            Return True
        End Function

    End Class

    Sub Main()
        Dim FA As New Finite_Automaton

        Dim file As Integer
        Dim path As String = Directory.GetCurrentDirectory()

        Dim FA_Type As Machine_Type

        Dim i As Integer
        For i = 0 To 14 'there are 15 machines to evaluate; this value could be altered to accomodate more or less machines
            FA.Clear()
            file = FreeFile()
            If i < 10 Then
                FileOpen(file, path + " /Automata Descriptions/fa0" + i.ToString() + ".fa", OpenMode.Input)
                Console.Write("Initializing FA 0" + i.ToString() + "...")
            Else
                FileOpen(file, path + "/Automata Descriptions/fa" + i.ToString() + ".fa", OpenMode.Input)
                Console.Write("Initializing FA " + i.ToString() + "...")
            End If

            FA.Initialize(file)

            FileClose(file) 'done with input file for this machine

            FA_Type = FA.ValidateMachine()
            FA.SetMachineType(FA_Type)

            Console.Write("done." + vbNewLine)

            Dim fileLog As Integer 'Log File needed for any validation result
            If FA_Type = Machine_Type.DFA Then
                Dim word As String
                Dim isAccepted As Boolean
                Dim numAcc As Integer = 0
                Dim numRej As Integer = 0
                Dim numWords As Integer = 0
                Dim fileAcc As Integer 'Accepted and Rejected Strings go to separate files (DFA only)
                Dim fileRej As Integer

                'open the necessary files
                If i < 10 Then
                    file = FreeFile()
                    FileOpen(file, path + "/Automata Descriptions/fa0" + i.ToString() + ".in", OpenMode.Input)
                    fileAcc = FreeFile()
                    FileOpen(fileAcc, path + "/Automata Descriptions/fa0" + i.ToString() + ".acc", OpenMode.Output)
                    fileRej = FreeFile()
                    FileOpen(fileRej, path + "/Automata Descriptions/fa0" + i.ToString() + ".rej", OpenMode.Output)
                    fileLog = FreeFile()
                    FileOpen(fileLog, path + "/Automata Descriptions/fa0" + i.ToString() + ".log", OpenMode.Output)
                    Console.Write("Simulating FA 0" + i.ToString() + "...")
                Else
                    file = FreeFile()
                    FileOpen(file, path + "/Automata Descriptions/fa" + i.ToString() + ".in", OpenMode.Input)
                    fileAcc = FreeFile()
                    FileOpen(fileAcc, path + "/Automata Descriptions/fa" + i.ToString() + ".acc", OpenMode.Output)
                    fileRej = FreeFile()
                    FileOpen(fileRej, path + "/Automata Descriptions/fa" + i.ToString() + ".rej", OpenMode.Output)
                    fileLog = FreeFile()
                    FileOpen(fileLog, path + "/Automata Descriptions/fa" + i.ToString() + ".log", OpenMode.Output)
                    Console.Write("Simulating FA " + i.ToString() + "...")
                End If

                'simulate each string on the DFA and echo to appropriate file
                Do While Not EOF(file)
                    word = LineInput(file)
                    numWords += 1
                    isAccepted = FA.EvaluateString(word)

                    If isAccepted Then
                        Print(fileAcc, word + vbNewLine)
                        numAcc += 1
                    Else
                        Print(fileRej, word + vbNewLine)
                        numRej += 1
                    End If
                Loop

                FileClose(fileAcc)
                FileClose(fileRej)
                FileClose(file)

                'Print the log file
                FA.PrintAlphabet(fileLog)
                FA.PrintNumStates(fileLog)
                Print(fileLog, "Valid: " + FA_Type.ToString() + vbNewLine)
                Print(fileLog, "Strings: " + numWords.ToString() + vbNewLine)
                Print(fileLog, "Accepted: " + numAcc.ToString() + vbNewLine)
                Print(fileLog, "Rejected: " + numRej.ToString() + vbNewLine)
                FileClose(fileLog)
            Else 'NFA or INVALID
                fileLog = FreeFile()
                If i < 10 Then
                    FileOpen(fileLog, path + "/Automata Descriptions/fa0" + i.ToString() + ".log", OpenMode.Output)
                    Console.Write("Simulating FA 0" + i.ToString() + "...")
                Else
                    FileOpen(fileLog, path + "/Automata Descriptions/fa" + i.ToString() + ".log", OpenMode.Output)
                    Console.Write("Simulating FA " + i.ToString() + "...")
                End If
                FA.PrintAlphabet(fileLog)
                FA.PrintNumStates(fileLog)
                Print(fileLog, "Valid: " + FA_Type.ToString() + vbNewLine)
                FileClose(fileLog)
                FileClose(fileLog)
            End If

            Console.Write("done." + vbNewLine)
        Next

        Console.Write("Simulation Complete.")

    End Sub

End Module
