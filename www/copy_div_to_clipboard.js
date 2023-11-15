function copy_div_to_clipboard( id ) {

var fake = document.createElement( 'textarea' );

fake.innerHTML = document.getElementById( id ).innerHTML;
document.body.appendChild( fake );

fake.select();
document.execCommand( 'copy' );
document.body.removeChild( fake );
}
