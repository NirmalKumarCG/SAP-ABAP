REPORT ZCHATGPT_API.

* Declare variables
DATA: lv_endpoint TYPE string VALUE '/v1/codegen',
      lv_api_key TYPE string VALUE 'your_api_key',
      lv_request_data TYPE string,
      lv_response TYPE string,
      lv_http_client TYPE REF TO cl_http_client,
      lv_http_response TYPE REF TO cl_http_response.

* Set the API key as a request header
lv_http_client = cl_http_client=>create( ).
lv_http_client->set_header( name = 'Authorization' value = lv_api_key ).

* Set the API endpoint URL and request method
lv_http_client->set_request_uri( lv_endpoint ).
lv_http_client->set_request_method( 'POST' ).

* Set the request data
lv_request_data = '{"model": "text-davinci-002", "prompt": "Write a function to calculate the factorial of a number", "max_tokens": 2048}'.
lv_http_client->set_request_data( lv_request_data ).

* Send the API request and receive the response
lv_http_client->send( ).
lv_http_response = lv_http_client->receive( ).

* Parse the API response
lv_response = lv_http_response->get_cdata( ).

* Deserialize the API response
DATA(lv_xml) = cl_sxml_string_reader=>create( lv_response ).
lv_xml->deserialize( ).

* Access the code element in the API response
DATA(lv_code) = lv_xml->get_first_child( ).

* Write the code to the console
WRTIE : / lv_code.


DATA: lv_http_client TYPE REF TO cl_http_client.

* Create an HTTP client object
lv_http_client = cl_http_client=>create( ).

* Set the API hostname and endpoint URL
lv_http_client->set_request_uri( 'https://api.openai.com/v1/codegen' ).

* Set the API key as a request header
lv_http_client->set_header( name = 'Authorization' value = 'your_api_key' ).

* Set the request method and data
lv_http_client->set_request_method( 'POST' ).
lv_http_client->set_request_data( '{"model": "text-davinci-002", "prompt": "Write a function to calculate the factorial of a number", "max_tokens": 2048}' ).

* Send the API request and receive the response
lv_http_client->send( ).
lv_http_response = lv_http_client->receive( ).

