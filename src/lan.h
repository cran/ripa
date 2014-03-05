/*
The header of an LAN image
*/
struct header_type{
	char descriptor[6];
	char ipack;
	int numbands;
	char blank1[4];
	int numcols;
	int numrows;
	int xstart;
	int ystart;
	char blank2[56];
	int maptype;
	int numclass;
	char blank3[10];
	float acre;
	float xmap;
	float ymap;
	float xcell;
	float ycell;
} header_lan;


