module OpilioCraft.FSharp.Prelude.LikeOperator

//open System.Collections.Generic

let private charListToSet (charList : string) =
    let rec transform chars cache : Set<char> =
        match chars with
        | x :: '-' :: y :: tail ->
            seq { x .. y }
            |> Seq.fold (fun cache c -> cache |> Set.add c) cache
            |> transform tail
        | x :: tail ->
            cache
            |> Set.add x
            |> transform tail
        | [] -> cache

    transform (charList.ToCharArray() |> Array.toList) Set.empty

//let isLike (text : string) (pattern : string) =
//    // Characters matched so far
//    int matched = 0;

//    // Loop through pattern string
//    for (int i = 0; i < pattern.Length; )
//    {
//        // Check for end of string
//        if (matched > s.Length)
//            return false;

//        // Get next pattern character
//        char c = pattern[i++];
//        if (c == '[') // Character list
//        {
//            // Test for exclude character
//            bool exclude = (i < pattern.Length && pattern[i] == '!');
//            if (exclude)
//                i++;
//            // Build character list
//            int j = pattern.IndexOf(']', i);
//            if (j < 0)
//                j = s.Length;
//            HashSet<char> charList = CharListToSet(pattern.Substring(i, j - i));
//            i = j + 1;

//            if (charList.Contains(s[matched]) == exclude)
//                return false;
//            matched++;
//        }
//        else if (c == '?') // Any single character
//        {
//            matched++;
//        }
//        else if (c == '#') // Any single digit
//        {
//            if (!Char.IsDigit(s[matched]))
//                return false;
//            matched++;
//        }
//        else if (c == '*') // Zero or more characters
//        {
//            if (i < pattern.Length)
//            {
//                // Matches all characters until
//                // next character in pattern
//                char next = pattern[i];
//                int j = s.IndexOf(next, matched);
//                if (j < 0)
//                    return false;
//                matched = j;
//            }
//            else
//            {
//                // Matches all remaining characters
//                matched = s.Length;
//                break;
//            }
//        }
//        else // Exact character
//        {
//            if (matched >= s.Length || c != s[matched])
//                return false;
//            matched++;
//        }
//    }
//    // Return true if all characters matched
//    return (matched == s.Length);
