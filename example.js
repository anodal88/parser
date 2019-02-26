Vue.component('my-custom-component', {
    data: function () {
        return {count: 0}
    }, props: ['name', 'lastname'], template: `<b>Test</b>`
})