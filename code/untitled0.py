print('Before start the software,do you need help with rules and regulation ?')
firstAns=input('Yes or No :')
print('')
print('')

if firstAns=='Yes' or 'yes' or 'Y' or 'y':
    print('Rule and regularation:')
    print('  Notice that we only have classify variables: C={Linear Algebra : 1,Calculus : 2,Probability : 3,Complex Analysis : 4}')
    print('  Please print the mathematics modules as above')
    print('  Otherwise the software will terminate')
if firstAns=='no' or 'nop' or 'N' or 'nono' or 'n':
    print('Loading the software..')

print('')
print('')
course_interest=input('Specify the relevant mathematics module:')
print('')
print('Module {} has been selected, we are now running the corrosponding software'.format(course_interest))
print('....')
print('...')
print('')
print('')
question=input('Please input the question:')
print('')
print('')
print('Wait for software execute...')
sec='Matrix Manipulation'
print('')
print('')
print('The question is been classified as a {} problem'.format(sec))

print('Here are some useful formula that might helps:')
print('')
print('A_ij transpose is A_ji and A tranpose = A if A is symmetry matrix')
print('')
ans=[[1,4],[2,5],[3,6]]
print('Predicted answer: is {}'.format(str(ans)))
#save the rules in a file we just simpy load in 

#_=input('Press [Enter] to exit the software.')