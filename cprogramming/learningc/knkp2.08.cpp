// KNK Chapter 2 Programming Project 5
// by orangecalculator

#include <cstdio>

int main(){
	float loan,interest,payment;
	
	printf("Enter amount of loan: ");
	scanf("%f",&loan);
	printf("Enter interest rate: ");
	scanf("%f",&interest);
	printf("Enter monthly payment: ");
	scanf("%f",&payment);
	
	interest = interest /100.0f /12.0f;
	loan=(loan)*(1.0f+interest)-payment;
	printf("\nBalance remaining after first payment: $%.2f",loan);
	loan=(loan)*(1.0f+interest)-payment;
	printf("\nBalance remaining after second payment: $%.2f",loan);
	loan=(loan)*(1.0f+interest)-payment;
	printf("\nBalance remaining after third payment: $%.2f",loan);
}
