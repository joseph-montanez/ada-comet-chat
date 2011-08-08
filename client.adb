with Ada.Containers.Indefinite_Vectors;

package body Client is

   function Get_Id (Self : Object) return String is
   begin
      return Self.Id;
   end Get_Id;

   procedure Set_Id (Self : in out Object; Id : String) is
   begin
      Self.Id := Id;
   end Set_Id;

   procedure Add_Buffer (Self : in out Object; Message : String) is
   begin
      Self.Buffer.Append (New_Item => Message);
   end Add_Buffer;
end Client;
