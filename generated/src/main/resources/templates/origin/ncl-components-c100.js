Vue.component('ncl-components-c100', {data: {},props:{pricingOffers:{ required: false , type: Array } , pricingDatePrices:{ required: true , type: Array } , pricingTitles:{ required: true , type: Array } , textAlert:{ required: false , type: String } , textAlertHeader:{ required: false , type: String } , classAlert:{ required: false , type: String } , btnLink:{ required: true , type: Array } , callToActionCard:{ required: true , type: Array } , offersList:{ required: true , type: Array } , offersTitle:{ required: true , type: String } , disclaimer:{ required: true , type: String } , merchandisingList:{ required: true , type: Array } , priceOffer:{ required: true , type: String } , labelTextOffer:{ required: true , type: String } , offerDisclaimer:{ required: false , type: String } , offer:{ required: false , type: String } , priceSail:{ required: true , type: String } , labelTextSail:{ required: true , type: String } , quantityPorts:{ required: false , type: Number } , embarkationPortList:{ required: true , type: Array } , summaryList:{ required: true , type: Array } , title:{ required: true , type: String } , embarkationPort:{ required: true , type: String } , destination:{ required: true , type: String } , ships:{ required: true , type: String } , duration:{ required: true , type: String } , favoriteIcon:{ required: true , type: String } , favoriteLink:{ required: true , type: Array } , linkSocial:{ required: true , type: Array } , favoriteStatus:{ required: true , type: String } , figcaptionType:{ required: false , type: Boolean } , figcaptionImage:{ required: false , type: Array } , imagePosition:{ required: false , type: Number } , desktopImage:{ required: true , type: Array }},computed:{
offer3: function () {
 return `       <ncl-components-c150 
	v-bind:class = "'-expandable'" 
	v-bind:titles = "pricingTitles" 
	v-bind:datePrices = "pricingDatePrices" 
	v-bind:offers = "pricingOffers" 
>
</ncl-components-c150>
   ` 
}, 
name: function () {
 return  'Antonio' 
}},template: `                                                                                                               <template v-if= "numMarbles ==  0">
             You have no marbles.            <template v-for=" i in range(numLines)">
              Line {{i + 1}} of {{numLines}}.<br>
            
</template>
         
</template>
<template v-else-if= "numMarbles ==  1 || numMarbles ==  2 || numMarbles ==  3">
             You have a normal number of marbles.         
</template>
<template v-else>
  // 4 || more    You have more marbles than you know what to do with.    
</template>
<template v-for=" operand in operands">
  <template v-if = " ! isFirst(operand)">
 + 
</template>
  {{operand}}
</template>
<template v-show="!( operands)">
  0
</template>
    <!--c100-->
    <article class="c100">
        <div class="c100_card">
            <div class="c100_body">
               <ncl-components-c94 
	v-bind:duration = "duration" 
	v-bind:summaryList = "summaryList" 
	v-bind:favoriteStatus = "favoriteStatus" 
	v-bind:ships = "ships" 
	v-bind:quantityPorts = "quantityPorts" 
	v-bind:favoriteLink = "favoriteLink" 
	v-bind:embarkationPortList = "embarkationPortList" 
	v-bind:figcaptionType = "figcaptionType" 
	v-bind:favoriteIcon = "favoriteIcon" 
	v-bind:dataTitle = "destination" 
	v-bind:figcaptionImage = "figcaptionImage" 
	v-bind:dataImage = "desktopImagesrc" 
	v-bind:dataUrl = "callToActionCardhref" 
	v-bind:desktopImage = "desktopImage" 
	v-bind:imagePosition = "imagePosition" 
	v-bind:title = "title" 
	v-bind:linkSocial = "linkSocial" 
	v-bind:destination = "destination" 
	v-bind:embarkationPort = "embarkationPort" 
>
</ncl-components-c94>
            
</div>
            <aside class="c100_aside">
               <ncl-components-c92 
	v-bind:alertClass = "classAlert" 
	v-bind:priceSail = "priceSail" 
	v-bind:priceOffer = "priceOffer" 
	v-bind:textAlertHeader = "textAlertHeader" 
	v-bind:labelTextSail = "labelTextSail" 
	v-bind:merchandisingList = "merchandisingList" 
	v-bind:labelTextOffer = "labelTextOffer" 
	v-bind:textAlert = "textAlert" 
	v-bind:callToActionCard = "callToActionCard" 
	v-bind:offer = "offer" 
>
</ncl-components-c92>
            
</aside>
        
</div>
        <template v-if = " disclaimer">
            <div class="c100_disclaimer">
                <span class="extra -small">
{{disclaimer}}
</span>
                <span class="extra -small">
{{ disclaimer_2}}
</span>
            
</div>
            
</template>
<template v-else-if = "disclaimer==2">
             <div class="c100_disclaimer">
                            <span class="extra -small">
{{disclaimer3}}
</span>
                            <span class="extra -small">
{{ disclaimer_3}}
</span>
                        
</div>
            
</template>
<template v-else-if = "disclaimer==3">
            <div class="c100_disclaimer">
                                        <span class="extra -small">
{{disclaimer4}}
</span>
                                        <span class="extra -small">
{{ disclaimer_4}}
</span>
                                    
</div>
                                    
</template>
<template v-else>
                                    elseblock sdfsdf sdf s        
</template>
        <div class="c100_modals">
        <ncl-components-c150 
	v-bind:class = "'-expandable'" 
	v-bind:titles = "pricingTitles" 
	v-bind:datePrices = "pricingDatePrices" 
	v-bind:offers = "pricingOffers" 
>
</ncl-components-c150>
           <ncl-components-c99 
	v-bind:duration = "duration" 
	v-bind:offerDisclaimer = "offerDisclaimer" 
	v-bind:ships = "ships" 
	v-bind:priceSail = "priceSail" 
	v-bind:btnLink = "btnLink" 
	v-bind:priceOffer = "priceOffer" 
	v-bind:labelTextSail = "labelTextSail" 
	v-bind:offersList = "offersList" 
	v-bind:labelTextOffer = "labelTextOffer" 
	v-bind:offersTitle = "offersTitle" 
	v-bind:class = "'-expandable'" 
	v-bind:offer = "offer" 
	v-bind:destination = "destination" 
	v-bind:embarkationPort = "embarkationPort" 
>
</ncl-components-c99>
           <ncl-components-c150 
	v-bind:class = "'-expandable'" 
	v-bind:titles = "pricingTitles" 
	v-bind:datePrices = "pricingDatePrices" 
	v-bind:offers = "pricingOffers" 
>
</ncl-components-c150>
        
</div>
    
</article>
`)