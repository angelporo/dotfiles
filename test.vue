<template>
  <div>
    <h1>Test Vue Component</h1>
    <p>This is a test Vue component created at {{ currentDate }}</p>
    <button @click="increment">Count: {{ count }}</button>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const count = ref(0)
const currentDate = computed(() => new Date().toLocaleString())

function increment() {
  count.value++
}
</script>

<style scoped>
div {
  padding: 1rem;
  font-family: sans-serif;
}
h1 {
  color: #42b983;
}
button {
  margin-top: 1rem;
  padding: 0.5rem 1rem;
  background: #42b983;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
}
button:hover {
  background: #33a06f;
}
</style>
