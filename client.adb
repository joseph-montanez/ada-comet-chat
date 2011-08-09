with Ada.Containers.Indefinite_Vectors;

package body Client is

   function Get_Client_Id (Self : Object) return String is
   begin
      return Self.Client_Id;
   end Get_Client_Id;

   function Get_Connection_Id (Self : Object) return String is
   begin
      return Self.Connection_Id;
   end Get_Connection_Id;

   procedure Set_Client_Id (Self : in out Object; Client_Id : String) is
   begin
      Self.Client_Id := Client_Id;
   end Set_Client_Id;

   procedure Set_Connection_Id (Self : in out Object; Connection_Id : String) is
   begin
      Self.Connection_Id := Connection_Id;
   end Set_Connection_Id;

   procedure Add_Buffer (Self : in out Object; Message : String) is
   begin
      Self.Buffer.Append (New_Item => Message);
   end Add_Buffer;
end Client;
