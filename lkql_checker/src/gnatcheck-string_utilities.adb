--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Gnatcheck.String_Utilities is

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (S : String) return String is
   begin
      return Result : String (S'Range) do
         for X in S'Range loop
            if X = S'First
              or else not (Is_Letter (S (X - 1)) or else Is_Digit (S (X - 1)))
            then
               Result (X) := To_Upper (S (X));
            else
               Result (X) := To_Lower (S (X));
            end if;
         end loop;
      end return;
   end Capitalize;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
      Result : constant String := X'Img;

   begin
      case Result (1) is
         when ' ' =>
            return Slide (Result (2 .. Result'Last));

         when '-' =>
            return Result;

         when others =>
            raise Program_Error;
      end case;
   end Image;

   function Image (X : Modular) return String is
      Result : constant String := X'Img;

   begin
      case Result (1) is
         when ' ' =>
            return Slide (Result (2 .. Result'Last));

         when '-' =>
            return Result;

         when others =>
            raise Program_Error;
      end case;
   end Image;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (X, Prefix : String) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         return To_Lower (X (X'First .. X'First + Prefix'Length - 1))
                  = To_Lower (Prefix);
      end if;

      return False;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (X, Suffix : String) return Boolean is
   begin
      if X'Length >= Suffix'Length then
         return To_Lower (X (X'Last - Suffix'Length + 1 .. X'Last))
                  = To_Lower (Suffix);
      end if;

      return False;
   end Has_Suffix;

   -----------
   -- Slide --
   -----------

   function Slide (X : String) return String is
   begin
      return Result : constant String (1 .. X'Length) := X;
   end Slide;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (FD : File_Descriptor) return String_Access is
      Length : constant Natural := Natural (File_Length (FD));

      This_Read : Integer;
      Read_Ptr  : Natural := 1;

      Buffer : constant String_Access := new String (1 .. Length);
   begin
      loop
         This_Read :=
           Read
             (FD,
              A => Buffer.all'Address,
              N => Length + 1 - Read_Ptr);
         Read_Ptr := Read_Ptr + Integer'Max (This_Read, 0);
         exit when This_Read <= 0 or else Read_Ptr = Length + 1;
      end loop;

      if Read_Ptr /= Length + 1 then
         raise Program_Error with "Read_File failed";
      end if;

      return Buffer;
   end Read_File;

   function Read_File (File_Name : String) return String_Access is
      FD : constant File_Descriptor := Open_Read (File_Name, Fmode => Binary);
   begin
      if FD = Invalid_FD then
         raise Program_Error with "file not found: " & File_Name;
      end if;

      return Result : constant String_Access := Read_File (FD) do
         Close (FD);
      end return;
   end Read_File;

   -------------------
   -- Remove_Spaces --
   -------------------

   function Remove_Spaces (S : String) return String is
      Result   : String (1 .. S'Length);
      Res_Last : Natural := 0;
   begin
      for J in S'Range loop
         if not Is_White_Space (S (J)) then
            Res_Last := Res_Last + 1;
            Result (Res_Last) := S (J);
         end if;
      end loop;

      return Result (1 .. Res_Last);
   end Remove_Spaces;

   -------------------
   -- Remove_Quotes --
   -------------------

   function Remove_Quotes (S : String) return String is
   begin
      if S'Length > 1 then
         declare
            First_Char : constant Character := S (S'First);
            Last_Char : constant Character := S (S'Last);
         begin
            if (First_Char = '"' and then Last_Char = '"')
              or else (First_Char = ''' and then Last_Char = ''')
            then
               return S (S'First + 1 .. S'Last - 1);
            end if;
         end;
      end if;

      return S;
   end Remove_Quotes;

   -----------
   -- Split --
   -----------

   function Split
     (S          : String;
      Sep        : Character;
      Trim_Elems : Boolean := False) return String_Vector
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings;

      Res : String_Vector;
      Acc : Unbounded_String;
   begin
      for C of S loop
         if C = Sep then
            Res.Append (To_String
              (if Trim_Elems then Trim (Acc, Both) else Acc));
            Set_Unbounded_String (Acc, "");
         else
            Append (Acc, C);
         end if;
      end loop;
      Res.Append (To_String (if Trim_Elems then Trim (Acc, Both) else Acc));
      return Res;
   end Split;

   ----------
   -- Join --
   ----------

   function Join (V : String_Vector; Sep : String) return String
   is
      use Ada.Strings;
      Res : Unbounded.Unbounded_String;
   begin
      for S of V loop
         if Unbounded.Length (Res) > 0 then
            Unbounded.Append (Res, Sep);
         end if;
         Unbounded.Append (Res, S);
      end loop;
      return Unbounded.To_String (Res);
   end Join;

   ------------------------------
   -- Simple_String_Dictionary --
   ------------------------------

   package body Simple_String_Dictionary is

      function Case_Insensitive_Equal (Left, Right : String) return Boolean is
      (Left'Length = Right'Length and then To_Lower (Left) = To_Lower (Right));
      --  Case insensitive string equality check

      package Dictionaries is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => Case_Insensitive_Equal,
         "="                 => Case_Insensitive_Equal);

      Dictionary : Dictionaries.Set;
      Iterator   : Dictionaries.Cursor := Dictionaries.No_Element;

      -----------------------
      -- Add_To_Dictionary --
      -----------------------

      procedure Add_To_Dictionary (S : String) is
      begin
         Dictionaries.Include (Dictionary, S);
      end Add_To_Dictionary;

      ------------
      --  Clear --
      ------------

      procedure Clear is
      begin
         Dictionaries.Clear (Dictionary);
      end Clear;

      ----------
      -- Done --
      ----------

      function Done return Boolean is
      begin
         return not Dictionaries.Has_Element (Iterator);
      end Done;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Dictionaries.Is_Empty  (Dictionary);
      end Is_Empty;

      ----------------------
      -- Is_In_Dictionary --
      ----------------------

      function Is_In_Dictionary (S : String) return Boolean is
      begin
         return Dictionaries.Contains (Dictionary, S);
      end Is_In_Dictionary;

      ----------------
      -- Next_Entry --
      ----------------

      function Next_Entry return String is
         Result : constant String := Dictionaries.Element (Iterator);
      begin
         Iterator := Dictionaries.Next (Iterator);
         return Result;
      end Next_Entry;

      ----------------------
      -- Print_Dictionary --
      ----------------------

      procedure Print_Dictionary is
         Next_Item : Dictionaries.Cursor;
         use Ada.Text_IO;
      begin
         Put_Line (Standard_Error, "Content of dictionary " & Dictionary_Name);

         if Is_Empty then
            Put_Line (Standard_Error, "Empty");
         else
            Next_Item := Dictionaries.First (Dictionary);

            while Dictionaries.Has_Element (Next_Item) loop
               Put      (Standard_Error, ">>");
               Put      (Standard_Error, Dictionaries.Element (Next_Item));
               Put_Line (Standard_Error, "<<");
               Next_Item := Dictionaries.Next (Next_Item);
            end loop;
         end if;
      end Print_Dictionary;

      ----------------------------
      -- Remove_From_Dictionary --
      ----------------------------

      procedure Remove_From_Dictionary (S : String) is
      begin
         Dictionaries.Exclude (Dictionary, S);
      end Remove_From_Dictionary;

      --------------------
      -- Reset_Iterator --
      --------------------

      procedure Reset_Iterator is
      begin
         Iterator := Dictionaries.First (Dictionary);
      end Reset_Iterator;

   end Simple_String_Dictionary;

end Gnatcheck.String_Utilities;
