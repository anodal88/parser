Vue.component('claimer==1-sclaimer==1', {    data: {},    props: [],    template: `<template v-if = "disclaimer==1">
   <span>
test
</span>
    <template v-if = "disclaimer">
       <div class="c100_disclaimer">
           <span class="extra -small">
{{disclaimer}}
</span>
       
</div>
   
</template>
{else}   <span>
else test ..
</span>
</template>
<template v-else-if = "disclaimer==5">
   <span>
else if test ..
</span>
</template>
`,    computed:{}})