// KNK Chapter 3 Programming Project 2
// by orangecalculator

#include <cstdio>

int main(){
	int number,month,day,year;
	float price;

	printf("Enter item number: ");
	scanf("%d",&number);
	printf("Enter unit price: ");
	scanf("%f",&price);
	printf("Enter purchase date (mm/dd/yyyy) : ");
	scanf("%d/%d/%d",&month,&day,&year);

	if(price>9999.99f){
	printf("Price cannot exceed $9999.99\n");
	return 0;
	}

	printf("Item\t\tUnit\t\tPurchase\n");
	printf("\t\tPrice\t\tDate\n");
	printf("%d\t\t$%7.2f\t%.2d/%.2d/%.4d",number,price,month,day,year);
}
