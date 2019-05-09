import React from "react";
import AvailableTimes from "react-available-times";

export default () => (
  <AvailableTimes
    weekStartsOn="monday"
    calendars={[
      {
        id: "work",
        title: "Work",
        foregroundColor: "#ff00ff",
        backgroundColor: "#f0f0f0",
        selected: true
      },
      {
        id: "private",
        title: "My private cal",
        foregroundColor: "#666",
        backgroundColor: "#f3f3f3"
      }
    ]}
    onChange={selections => {
      selections.forEach(({ start, end }) => {
        console.log("Start:", start, "End:", end);
      });
    }}
    onEventsRequested={({ calendarId, start, end, callback }) => {
      console.log(calendarId, start, end, callback);
    }}
    initialSelections={[
      { start: new Date(), end: new Date(new Date().getTime() + 10000) }
    ]}
    height={400}
    recurring={false}
    availableDays={["monday", "tuesday", "wednesday", "thursday", "friday"]}
    availableHourRange={{ start: 9, end: 19 }}
  />
);
