#define NSC __attribute__((cmse_nonsecure_entry))
#define NS  __attribute__((cmse_nonsecure_call))


int NSC add10(int a){
    return 10+a;
}