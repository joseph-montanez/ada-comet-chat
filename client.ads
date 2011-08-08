with Ada.Containers.Indefinite_Vectors;

package Client is
   package Buffer_Container is new Ada.Containers.Indefinite_Vectors (
      Index_Type => Natural,
      Element_Type => String
   );
   type Object is tagged record
      -- Id is the socket identifier.
      Id     : String (1 .. 32);
      -- Buffer is used to hold messages until they are ready to be sent.
      Buffer : Buffer_Container.Vector;
   end record;
   
   type Object_Access is access constant Object;

   function Get_Id (Self : Object) return String;

   procedure Set_Id (Self : in out Object; Id : String);

   procedure Add_Buffer (Self : in out Object; Message : String);
end Client;