Vue.component('ncl-components-c100', {
    data: {},
    props: ['desktopImage', 'figcaptionImage', 'linkSocial', 'favoriteIcon', 'embarkationPortList', 'disclaimer', 'pricingDatePrices', '', 'pricingOffers'],
    template: `                                                                                                          <!--c100-->    
<article class="c100">
   <div class="c100_card">
      <div class="c100_body">
         <ncl-components-c94 
            v-bind:duration = duration
            v-bind:summaryList = summaryList
            v-bind:favoriteStatus = favoriteStatus
            v-bind:ships = ships
            v-bind:quantityPorts = quantityPorts
            v-bind:favoriteLink = favoriteLink
            v-bind:embarkationPortList = embarkationPortList
            v-bind:figcaptionType = figcaptionType
            v-bind:favoriteIcon = favoriteIcon
            v-bind:dataTitle = destination
            v-bind:figcaptionImage = figcaptionImage
            v-bind:dataImage = desktopImagesrc
            v-bind:dataUrl = callToActionCardhref
            v-bind:desktopImage = desktopImage
            v-bind:imagePosition = imagePosition
            v-bind:title = title
            v-bind:linkSocial = linkSocial
            v-bind:destination = destination
            v-bind:embarkationPort = embarkationPort
            ></ncl-components-c94>
      </div>
      <aside class="c100_aside">
         <ncl-components-c92 
            v-bind:alertClass = classAlert
            v-bind:priceSail = priceSail
            v-bind:priceOffer = priceOffer
            v-bind:textAlertHeader = textAlertHeader
            v-bind:labelTextSail = labelTextSail
            v-bind:merchandisingList = merchandisingList
            v-bind:labelTextOffer = labelTextOffer
            v-bind:textAlert = textAlert
            v-bind:callToActionCard = callToActionCard
            v-bind:offer = offer
            ></ncl-components-c92>
      </aside>
   </div>
   <div v-if= "disclaimer">
      <div class="c100_disclaimer">                <span class="extra -small">{{disclaimer}}</span>            </div>
   </div>
   <div class="c100_modals">
      <ncl-components-c99 
         v-bind:duration = duration
         v-bind:offerDisclaimer = offerDisclaimer
         v-bind:ships = ships
         v-bind:priceSail = priceSail
         v-bind:btnLink = btnLink
         v-bind:priceOffer = priceOffer
         v-bind:labelTextSail = labelTextSail
         v-bind:offersList = offersList
         v-bind:labelTextOffer = labelTextOffer
         v-bind:offersTitle = offersTitle
         v-bind:class = '-expandable'
         v-bind:offer = offer
         v-bind:destination = destination
         v-bind:embarkationPort = embarkationPort
         ></ncl-components-c99>
      <ncl-components-c150 
         v-bind:class = '-expandable'
         v-bind:titles = pricingTitles
         v-bind:datePrices = pricingDatePrices
         v-bind:offers = pricingOffers
         ></ncl-components-c150>
   </div>
</article>
`,
    computed: {}
})